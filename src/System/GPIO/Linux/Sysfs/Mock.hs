{-|
Module      : System.GPIO.Linux.Sysfs.Mock
Description : A mock MonadSysfs instance.
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

A mock 'MonadSysfs' instance, for testing GPIO programs.

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module System.GPIO.Linux.Sysfs.Mock
       ( -- * The SysfsMock monad
         SysfsMockT(..)
       , runSysfsMockT
       , runSysfsMock
       , evalSysfsMock
       , execSysfsMock
         -- * SysfsMock types
       , MockPinState(..)
       , defaultMockPinState
       , MockGpioChip(..)
       , MockPins
       , MockState(..)
         -- * Mock @sysfs@ operations
       , doesDirectoryExist
       , doesFileExist
       , getDirectoryContents
       , readFile
       , writeFile
       , unlockedWriteFile
       , pollFile
         -- * The initial @sysfs@ mock filesystem
       , sysfsRoot
       , sysfsRootZipper
         -- * Mock @sysfs@ exceptions
       , MockFSException(..)
       ) where

import Prelude hiding (readFile, writeFile)
import Control.Applicative (Alternative)
import Control.Monad.Catch
import Control.Monad.Catch.Pure (Catch, runCatch)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State.Strict
import Control.Monad.Writer (MonadWriter(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8 (pack, unlines)
import Data.Maybe (fromJust, isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, insertLookupWithKey)
import Foreign.C.Types (CInt(..))
import System.FilePath ((</>), splitFileName)
import System.GPIO.Linux.Sysfs.Mock.Internal (Directory, File(..), FileType(..), MockFSZipper, MockFSException(..), directory, dirName, files, subdirs, findFile')
import qualified System.GPIO.Linux.Sysfs.Mock.Internal as Internal (cd, mkdir, mkfile, pathFromRoot)
import System.GPIO.Linux.Sysfs.Monad (MonadSysfs)
import qualified System.GPIO.Linux.Sysfs.Monad as M (MonadSysfs(..))
import System.GPIO.Linux.Sysfs.Types (SysfsEdge(..))
import System.GPIO.Linux.Sysfs.Util (sysfsPath, intToByteString)
import System.GPIO.Types (Pin(..), PinDirection(..), PinValue(..))

-- | A mock pin.
--
-- Note that in the real Linux @sysfs@, pins keep their state even
-- after they're unexported.
data MockPinState =
  MockPinState {_direction :: Maybe PinDirection
               ,_activeLow :: !Bool
               ,_value :: !PinValue -- This is the line level
               ,_edge :: Maybe SysfsEdge}
  deriving (Show,Eq)

-- | Default initial state of mock pins.
defaultMockPinState :: MockPinState
defaultMockPinState =
  MockPinState {_direction = Just Out
               ,_activeLow = False
               ,_value = Low
               ,_edge = Just None}

-- | A mock "gpiochip".
--
-- Note that the '_initialPinStates' list is only used to construct a
-- mock filesystem. For each 'MockPinState' value in the list, a mock
-- pin is created in the mock filesystem with the path
-- @/sys/class/gpio/gpioN@, where @N@ is @_base@ + the pin's index in
-- the '_initialPinStates' list.
data MockGpioChip =
  MockGpioChip {_label :: String
               ,_base :: Int
               ,_initialPinStates :: [MockPinState]}
  deriving (Show,Eq)

-- | A type alias for a strict map of 'Pin' to its 'MockPinState'.
type MockPins = Map Pin MockPinState

-- | The global state of the mock @sysfs@ filesystem.
data MockState =
  MockState {_zipper :: MockFSZipper
            ,_pins :: MockPins}
  deriving (Show,Eq)

-- | A monad transformer which adds mock @sysfs@ computations to an
-- inner monad 'm'.
newtype SysfsMockT m a =
  SysfsMockT {unSysfsMockT :: StateT MockState m a}
  deriving (Alternative,Applicative,Functor,Monad,MonadFix,MonadIO,MonadThrow,MonadCatch,MonadMask,MonadState MockState,MonadReader r,MonadWriter w)

zipper :: (Monad m) => SysfsMockT m MockFSZipper
zipper = gets _zipper

putZipper :: (Monad m) => MockFSZipper -> SysfsMockT m ()
putZipper z =
  do s <- get
     put $ s {_zipper = z}

pins :: (Monad m) => SysfsMockT m MockPins
pins = gets _pins

putPins :: (Monad m) => MockPins -> SysfsMockT m ()
putPins ps =
  do s <- get
     put $ s {_pins = ps}

-- | Run a mock @sysfs@ computation in monad 'm' with an initial
-- filesystem ('MockFSZipper') and list of 'MockGpioChip'; and return
-- a tuple containing the computation's value and the final
-- 'MockState'. If an exception occurs in the mock computation, a
-- 'MockFSException' is thrown.
--
-- Before running the computation, the mock filesystem is populated
-- with the GPIO pins as specified by the list of 'MockGpioChip's. If
-- any of the chips in the list are already present in the filesystem,
-- or if any of the chips' pin ranges overlap, a 'MockFSException' is
-- thrown.
runSysfsMockT :: (MonadThrow m) => SysfsMockT m a -> MockFSZipper -> [MockGpioChip] -> m (a, MockState)
runSysfsMockT action startfs chips =
  do startState <- execStateT (unSysfsMockT $ pushd "/" (makeFileSystem chips)) (MockState startfs Map.empty)
     runStateT (unSysfsMockT action) startState

-- | The simplest possible (pure) mock @sysfs@ monad.
type SysfsMock a = SysfsMockT Catch a

-- | Run a 'SysfsMock' computation with an initial filesystem and list
-- of 'MockGpioChip's, and return a tuple containing the computation's
-- value and the final 'MockState'. Any exceptions that occur in the
-- mock computation are returned as a 'Left' value.
--
-- Before running the computation, the mock filesystem is populated
-- with the GPIO pins as specified by the list of 'MockGpioChip's. If
-- any of the chips in the list are already present in the filesystem,
-- or if any of the chips' pin ranges overlap, an error is returned.
runSysfsMock :: SysfsMock a -> MockFSZipper -> [MockGpioChip] -> Either MockFSException (a, MockState)
runSysfsMock a z chips =
  -- The 'MonadThrow' instance for 'Either' 'e' requires that 'e' '~'
  -- 'SomeException', and 'SomeException' has no 'Eq' instance, which
  -- makes this monad not very useful for testing. Therefore, we convert the
  -- exception type back to 'MockFSException'.
  case runCatch $ runSysfsMockT a z chips of
    Right result -> return result
    Left e ->
      -- Should be safe as there's no other exception type in this
      -- stack.
      Left $ fromJust $ fromException e

-- | Run a 'SysfsMock' computation with an initial filesystem and list
-- of 'MockGpioChip's, and return the computation's value, discarding
-- the final state. Any exceptions that occur in the mock computation
-- are returned as a 'Left' value.
--
-- Before running the computation, the mock filesystem is populated
-- with the GPIO pins as specified by the list of 'MockGpioChip's. If
-- any of the chips in the list are already present in the filesystem,
-- or if any of the chips' pin ranges overlap, an error is returned.
evalSysfsMock :: SysfsMock a -> MockFSZipper -> [MockGpioChip] -> Either MockFSException a
evalSysfsMock a z chips = fst <$> runSysfsMock a z chips

-- | Run a 'SysfsMock' computation with an initial filesystem and list
-- of 'MockGpioChip's, and return the final 'MockState', discarding
-- the computation's value. Any exceptions that occur in the mock
-- computation are returned as a 'Left' value.
--
-- Before running the computation, the mock filesystem is populated
-- with the GPIO pins as specified by the list of 'MockGpioChip's. If
-- any of the chips in the list are already present in the filesystem,
-- or if any of the chips' pin ranges overlap, an error is returned.
execSysfsMock :: SysfsMock a -> MockFSZipper -> [MockGpioChip] -> Either MockFSException MockState
execSysfsMock a z chips = snd <$> runSysfsMock a z chips

instance (MonadSysfs m, MonadThrow m) => M.MonadSysfs (SysfsMockT m) where
  doesDirectoryExist = doesDirectoryExist
  doesFileExist = doesFileExist
  getDirectoryContents = getDirectoryContents
  readFile = readFile
  writeFile = writeFile
  unlockedWriteFile = unlockedWriteFile
  pollFile = pollFile

makeFileSystem :: (MonadThrow m) => [MockGpioChip] -> SysfsMockT m MockFSZipper
makeFileSystem chips =
  do mapM_ makeChip chips
     zipper

makeChip :: (MonadThrow m) => MockGpioChip -> SysfsMockT m ()
makeChip chip =
  let chipdir = sysfsPath </> ("gpiochip" ++ show (_base chip))
  in
    do result <- addPins (_base chip) (_initialPinStates chip) <$> pins
       putPins result
       mkdir chipdir
       mkfile (chipdir </> "base") (Const [intToByteString $ _base chip]) False
       mkfile (chipdir </> "ngpio") (Const [intToByteString $ length (_initialPinStates chip)]) False
       mkfile (chipdir </> "label") (Const [C8.pack $ _label chip]) False

addPins :: Int -> [MockPinState] -> MockPins -> MockPins
addPins base states pm = foldr addPin pm (zip (map Pin [base..]) states)

addPin :: (Pin, MockPinState) -> MockPins -> MockPins
addPin (pin, st) pm =
  let insertLookup = Map.insertLookupWithKey (\_ a _ -> a)
  in
    case insertLookup pin st pm of
      (Nothing, newPm) -> newPm
      (Just _, _) -> error "Nope" -- Left $ PinAlreadyExists pin

pushd :: (MonadThrow m) => FilePath -> SysfsMockT m a -> SysfsMockT m a
pushd path action =
  do z <- zipper
     let restorePath = Internal.pathFromRoot z
     cd path >>= putZipper
     result <- action
     cd restorePath >>= putZipper
     return result

cd :: (MonadThrow m) => FilePath -> SysfsMockT m MockFSZipper
cd name =
  do fsz <- zipper
     case Internal.cd name fsz of
       Left e -> throwM e
       Right newz -> return newz

mkdir :: (MonadThrow m) => FilePath -> SysfsMockT m ()
mkdir path =
  let (parentName, childName) = splitFileName path
  in
    do parent <- cd parentName
       either throwM putZipper (Internal.mkdir childName parent)

mkfile :: (MonadThrow m) => FilePath -> FileType -> Bool -> SysfsMockT m ()
mkfile path filetype clobber =
  let (parentName, childName) = splitFileName path
  in
    do parent <- cd parentName
       either throwM putZipper (Internal.mkfile childName filetype clobber parent)

doesDirectoryExist :: (Monad m) => FilePath -> SysfsMockT m Bool
doesDirectoryExist path =
  do cwd <- zipper
     return $ either (const False) (const True) (Internal.cd path cwd)

doesFileExist :: (Monad m) => FilePath -> SysfsMockT m Bool
doesFileExist path =
  let (dirPath, fileName) = splitFileName path
  in
    do cwd <- zipper
       case Internal.cd dirPath cwd of
         Left _ -> return False
         Right (parent, _) ->
           return $ isJust (findFile' fileName parent)

getDirectoryContents :: (MonadThrow m) => FilePath -> SysfsMockT m [FilePath]
getDirectoryContents path =
  do parent <- fst <$> cd path
     return $ fmap dirName (subdirs parent) ++ fmap _fileName (files parent)

readFile :: (MonadThrow m) => FilePath -> SysfsMockT m ByteString
readFile path =
  fileAt path >>= \case
    Nothing -> throwM $ NotAFile path
    Just (Const contents) -> return $ C8.unlines contents
    Just _ -> throwM $ ReadError path

writeFile :: (MonadThrow m) => FilePath -> ByteString -> SysfsMockT m ()
writeFile path bs =
  fileAt path >>= \case
    Nothing -> throwM $ NotAFile path
    Just Export -> throwM $ ReadError path
    Just Unexport -> throwM $ ReadError path
    Just _ -> throwM $ WriteError path

fileAt :: (MonadThrow m) => FilePath -> SysfsMockT m (Maybe FileType)
fileAt path =
  let (dirPath, fileName) = splitFileName path
  in
    do parent <- fst <$> cd dirPath
       return $ findFile' fileName parent

unlockedWriteFile :: (MonadThrow m) => FilePath -> ByteString -> SysfsMockT m ()
unlockedWriteFile = writeFile

pollFile :: (Monad m) => FilePath -> Int -> SysfsMockT m CInt
pollFile _ _ = return 1

-- | The initial directory structure of a @sysfs@ GPIO filesystem.
sysfsRoot :: Directory
sysfsRoot =
  directory "/"
            []
            [directory "sys"
                       []
                       [directory "class"
                                  []
                                  [directory "gpio"
                                             [File "export" Export
                                             ,File "unexport" Unexport]
                                             []]]]

-- | The initial @sysfs@ filesystem zipper.
sysfsRootZipper :: MockFSZipper
sysfsRootZipper = (sysfsRoot, [])
