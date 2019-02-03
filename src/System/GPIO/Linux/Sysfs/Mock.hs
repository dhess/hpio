{-|
Module      : System.GPIO.Linux.Sysfs.Mock
Description : A mock MonadSysfs instance.
Copyright   : (c) 2019, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

A mock 'M.MonadSysfs' instance, for testing GPIO programs.

Note that this monad only mocks the subset of @sysfs@ functionality
required for GPIO programs. It does not mock the entire @sysfs@
filesystem.

-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

module System.GPIO.Linux.Sysfs.Mock
       ( -- * SysfsMock types
         MockWorld
       , MockPinState(..)
       , defaultMockPinState
       , logicalValue
       , setLogicalValue
       , MockGpioChip(..)
       , MockPins
       , mockWorldPins
       , initialMockWorld
         -- * The SysfsMock monad
       , SysfsMockT(..)
       , runSysfsMockT
       , evalSysfsMockT
       , execSysfsMockT
         -- * Run mock GPIO computations
       , SysfsGpioMock
       , runSysfsGpioMock
       , evalSysfsGpioMock
       , execSysfsGpioMock
       , SysfsGpioMockIO
       , runSysfsGpioMockIO
       , evalSysfsGpioMockIO
       , execSysfsGpioMockIO
         -- * Mock @sysfs@ exceptions.
       , MockFSException(..)
         -- * Run mock @sysfs@ computations.
         --
         -- | Generally speaking, you should not need to use these
         -- types, as they're not very useful on their own. They are
         -- primarily exported for unit testing.
         --
         -- If you want to run mock GPIO computations, use
         -- 'SysfsMockT' for buildling transformer stacks, or either
         -- 'SysfsGpioMock' or 'SysfsGpioMockIO' for simple
         -- computations that are pure or mix with 'IO', respectively.
       , SysfsMock
       , runSysfsMock
       , evalSysfsMock
       , execSysfsMock
       , SysfsMockIO
       , runSysfsMockIO
       , evalSysfsMockIO
       , execSysfsMockIO
         -- * Mock @sysfs@ actions
         --
         -- | Generally speaking, you should not need these actions.
         -- They are primarily exported for unit testing.
       , doesDirectoryExist
       , doesFileExist
       , getDirectoryContents
       , readFile
       , writeFile
       , unlockedWriteFile
       , pollFile
       ) where

import Protolude
       hiding (StateT, execStateT, readFile, runStateT, writeFile)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM)
import Control.Monad.Catch.Pure (Catch, runCatch)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.State.Strict (StateT(..), execStateT)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Control
       (ComposeSt, MonadBaseControl(..), MonadTransControl(..),
        defaultLiftBaseWith, defaultLiftWith, defaultRestoreM,
        defaultRestoreT)
import Control.Monad.Writer (MonadWriter(..))
import qualified Data.ByteString.Char8 as C8 (unlines)
import Data.List (length)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, insert, insertLookupWithKey, lookup)
import Data.Text (unwords)
import Foreign.C.Types (CInt(..))
import GHC.IO.Exception (IOErrorType(..))
import System.FilePath ((</>), splitFileName)
import System.IO.Error (IOError, mkIOError)

import System.GPIO.Linux.Sysfs.Mock.Internal
       (Directory, File(..), FileType(..), MockFSZipper(..), directory,
        dirName, files, subdirs, findFile)
import qualified System.GPIO.Linux.Sysfs.Mock.Internal as Internal
       (cd, mkdir, mkfile, pathFromRoot, rmdir)
import System.GPIO.Linux.Sysfs.Monad (SysfsGpioT(..))
import qualified System.GPIO.Linux.Sysfs.Monad as M (MonadSysfs(..))
import System.GPIO.Linux.Sysfs.Types (SysfsEdge(..))
import System.GPIO.Linux.Sysfs.Util
       (bsToInt, intToBS, pinActiveLowFileName, pinDirectionFileName,
        pinEdgeFileName, pinValueFileName, pinDirName, activeLowToBS,
        bsToActiveLow, pinDirectionToBS, bsToPinDirection, sysfsEdgeToBS,
        bsToSysfsEdge, pinValueToBS, bsToPinValue, sysfsPath)
import System.GPIO.Types
       (Pin(..), PinDirection(..), PinValue(..), gpioExceptionToException,
        gpioExceptionFromException, invertValue)

-- | A mock pin.
data MockPinState =
  MockPinState {_direction :: !PinDirection
               -- ^ The pin's direction
               ,_userVisibleDirection :: !Bool
               -- ^ Is the pin's direction visible from the filesystem?
               ,_activeLow :: !Bool
               -- ^ Is the pin configured as active-low?
               ,_value :: !PinValue
               -- ^ The pin's /physical/ signal level
               ,_edge :: Maybe SysfsEdge
               -- ^ The pin's interrupt mode (if supported)
               }
  deriving (Show,Eq)

-- | Linux @sysfs@ GPIO natively supports active-low logic levels. A
-- pin's "active" level is controlled by the pin's @active_low@
-- attribute. The pin's value relative to its @active_low@ attribute
-- is called its /logical value/. This function returns the mock pin's
-- logical value.
--
-- >>> logicalValue defaultMockPinState
-- Low
-- >>> logicalValue defaultMockPinState { _value = High }
-- High
-- >>> logicalValue defaultMockPinState { _activeLow = True }
-- High
-- >>> logicalValue defaultMockPinState { _activeLow = True, _value = High }
-- Low
logicalValue :: MockPinState -> PinValue
logicalValue s
  | _activeLow s = invertValue $ _value s
  | otherwise = _value s

-- | This function sets the 'MockPinState' signal level to the given
-- /logical/ value.
--
-- >>> _value $ setLogicalValue High defaultMockPinState
-- High
-- >>> _value $ setLogicalValue High defaultMockPinState { _activeLow = True }
-- Low
setLogicalValue :: PinValue -> MockPinState -> MockPinState
setLogicalValue v s
  | _activeLow s = s {_value = invertValue v}
  | otherwise = s {_value = v}

-- | Default initial state of mock pins.
--
-- >>> defaultMockPinState
-- MockPinState {_direction = Out, _userVisibleDirection = True, _activeLow = False, _value = Low, _edge = Just None}
defaultMockPinState :: MockPinState
defaultMockPinState =
  MockPinState {_direction = Out
               ,_userVisibleDirection = True
               ,_activeLow = False
               ,_value = Low
               ,_edge = Just None}

-- | A mock GPIO "chip." In the Linux @sysfs@ GPIO filesystem, a GPIO
-- chip is a set of one or more GPIO pins.
--
-- Note that the '_initialPinStates' list is used to construct the pin
-- state for a 'MockWorld' (see 'runSysfsMockT'). For each
-- 'MockPinState' value in the list, a mock pin will be created in the
-- mock filesystem such that, when that pin is exported, its path is
-- @\/sys\/class\/gpio\/gpioN@, where @N@ is @_base@ + the pin's index
-- in the '_initialPinStates' list.
data MockGpioChip =
  MockGpioChip {_label :: !Text
               -- ^ The name given to the chip in the filesystem
               ,_base :: !Int
               -- ^ The pin number of the chip's first pin
               ,_initialPinStates :: [MockPinState]
               -- ^ The pins' initial states
               }
  deriving (Show,Eq)

-- | A type alias for a strict map of 'Pin' to its 'MockPinState'.
type MockPins = Map Pin MockPinState

-- | The global state of a mock Linux GPIO subsystem with a @sysfs@
-- interface. It consists of the mock @sysfs@ GPIO filesystem state,
-- along with the state of every mock pin.
--
-- An actual Linux @sysfs@ GPIO filesystem is not like a
-- general-purpose filesystem. The user cannot create files or
-- directories directly; they can only be created (or modified) via
-- prescribed operations on special conrol files, which are themselves
-- created by the kernel.
--
-- Likewise, the kernel and hardware platform together determine which
-- GPIO pins are exposed to the user via the @sysfs@ GPIO filesystem.
--
-- To preserve the illusion of an actual @sysfs@ GPIO filesystem, the
-- 'MockWorld' type is opaque and can only be manipulated via the
-- handful of actions that are implemented in this module. These
-- actions have been designed to keep the internal state of the mock
-- @sysfs@ GPIO filesystem consistent with the behavior that would be
-- seen in an actual @sysfs@ GPIO filesystem.
--
-- The high/low signal level on a real GPIO pin can, of course, be
-- manipulated by the circuit to which the pin is conected. A future
-- version of this implementation may permit the direct manipulation
-- of mock pin values in order to simulate simple circuits, but
-- currently the only way to manipulate pin state is via the mock
-- @sysfs@ GPIO filesystem.
data MockWorld =
  MockWorld {_zipper :: MockFSZipper
            ,_pins :: MockPins}
  deriving (Show,Eq)

-- | Get the pin map from a 'MockWorld'.
mockWorldPins :: MockWorld -> MockPins
mockWorldPins = _pins

-- | The initial 'MockWorld', representing a @sysfs@ filesystem with
-- no pins.
initialMockWorld :: MockWorld
initialMockWorld = MockWorld sysfsRootZipper Map.empty

-- | A monad transformer which adds mock @sysfs@ computations to an
-- inner monad 'm'.
newtype SysfsMockT m a = SysfsMockT
  { unSysfsMockT :: StateT MockWorld m a
  } deriving ( Functor
             , Alternative
             , Applicative
             , Monad
             , MonadBase b
             , MonadFix
             , MonadPlus
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadCont
             , MonadIO
             , MonadReader r
             , MonadError e
             , MonadWriter w
             , MonadState MockWorld
             , MonadLogger
             , MonadLoggerIO
             , MonadTrans
             )

instance MonadBaseControl b m => MonadBaseControl b (SysfsMockT m) where
  type StM (SysfsMockT m) a = ComposeSt SysfsMockT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadTransControl SysfsMockT where
  type StT SysfsMockT a = StT (StateT MockWorld) a
  liftWith = defaultLiftWith SysfsMockT unSysfsMockT
  restoreT = defaultRestoreT SysfsMockT
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

type MockM m = (Functor m, MonadThrow m)

getZipper :: (MockM m) => SysfsMockT m MockFSZipper
getZipper = gets _zipper

putZipper :: (MockM m) => MockFSZipper -> SysfsMockT m ()
putZipper z =
  do s <- get
     put $ s {_zipper = z}

getPins :: (MockM m) => SysfsMockT m MockPins
getPins = gets _pins

pinState :: (MockM m) => Pin -> SysfsMockT m MockPinState
pinState pin =
  Map.lookup pin <$> getPins >>= \case
    Nothing -> throwM $ InternalError $
      unwords ["An operation attempted to get the mock pin state for non-existent pin", show pin]
    Just s -> return s

putPins :: (MockM m) => MockPins -> SysfsMockT m ()
putPins ps =
  do s <- get
     put $ s {_pins = ps}

putPinState :: (MockM m) => Pin -> (MockPinState -> MockPinState) -> SysfsMockT m ()
putPinState pin f =
  do ps <- pinState pin
     (Map.insert pin (f ps) <$> getPins) >>= putPins

-- | Run a mock @sysfs@ computation in monad 'm' with an initial mock
-- world and list of 'MockGpioChip's; and return a tuple containing the
-- computation's value and the final 'MockWorld'. If an exception
-- occurs in the mock computation, a 'MockFSException' is thrown.
--
-- Before running the computation, the 'MockWorld' is populated with
-- the GPIO pins as specified by the list of 'MockGpioChip's. If any
-- of the chips' pin ranges overlap, a 'MockFSException' is thrown.
--
-- Typically, you will only need this action if you're trying to mock
-- Linux @sysfs@ GPIO computations using a custom monad transformer
-- stack. For simple cases, see 'runSysfsGpioMock' or
-- 'runSysfsGpioMockIO'.
runSysfsMockT :: (MockM m) => SysfsMockT m a -> MockWorld -> [MockGpioChip] -> m (a, MockWorld)
runSysfsMockT action world chips =
  do startState <- execStateT (unSysfsMockT $ pushd "/" (makeFileSystem chips)) world
     runStateT (unSysfsMockT action) startState

-- | Like 'runSysfsMockT', but returns only the computation's value.
evalSysfsMockT :: (MockM m) => SysfsMockT m a -> MockWorld -> [MockGpioChip] -> m a
evalSysfsMockT a w chips = fst <$> runSysfsMockT a w chips

-- | Like 'runSysfsMockT', but returns only the final 'MockWorld'.
execSysfsMockT :: (MockM m) => SysfsMockT m a -> MockWorld -> [MockGpioChip] -> m MockWorld
execSysfsMockT a w chips = snd <$> runSysfsMockT a w chips

instance (MockM m) => M.MonadSysfs (SysfsMockT m) where
  doesDirectoryExist = doesDirectoryExist
  doesFileExist = doesFileExist
  getDirectoryContents = getDirectoryContents
  readFile = readFile
  writeFile = writeFile
  unlockedWriteFile = unlockedWriteFile
  pollFile = pollFile

-- | The simplest possible (pure) mock @sysfs@ monad.
--
-- NB: this monad /cannot/ run GPIO computations; its only use is to
-- mock @sysfs@ operations on an extremely limited mock @sysfs@
-- simulator.
--
-- You probably do not want to use this monad; see either
-- 'SysfsGpioMock' or 'SysfsGpioMockIO', which adds GPIO computations
-- to this mock @sysfs@ environment.
type SysfsMock = SysfsMockT Catch

-- | A pure version of 'runSysfsMockT' which returns errors in a
-- 'Left', and both the computation's value and the final state of the
-- 'MockWorld' in a 'Right'.
--
-- >>> let mockChip = MockGpioChip "chip0" 0 (replicate 16 defaultMockPinState)
-- >>> fst <$> runSysfsMock (getDirectoryContents "/sys/class/gpio") initialMockWorld [mockChip]
-- Right ["gpiochip0","export","unexport"]
-- >>> runSysfsMock (getDirectoryContents "/sys/class/does_not_exist") initialMockWorld [mockChip]
-- Left /sys/class/does_not_exist: Mock.Internal.cd: does not exist
runSysfsMock :: SysfsMock a -> MockWorld -> [MockGpioChip] -> Either SomeException (a, MockWorld)
runSysfsMock a w chips = runCatch $ runSysfsMockT a w chips

-- | Like 'runSysfsMock', but returns only the computation's value.
evalSysfsMock :: SysfsMock a -> MockWorld -> [MockGpioChip] -> Either SomeException a
evalSysfsMock a w chips = fst <$> runSysfsMock a w chips

-- | Like 'runSysfsMock', but returns only the final 'MockWorld'.
execSysfsMock :: SysfsMock a -> MockWorld -> [MockGpioChip] -> Either SomeException MockWorld
execSysfsMock a w chips = snd <$> runSysfsMock a w chips

-- | A specialization of 'SysfsGpioT' which runs (pure, fake) GPIO
-- computations via a mock @sysfs@.
type SysfsGpioMock = SysfsGpioT SysfsMock

-- | Run a 'SysfsGpioMock' computation with an initial mock world and
-- list of 'MockGpioChip's, and return a tuple containing the
-- computation's value and the final 'MockWorld'. Any exceptions that
-- occur in the mock computation are returned as a 'Left' value.
--
-- Before running the computation, the 'MockWorld' is populated with
-- the GPIO pins as specified by the list of 'MockGpioChip's. If any
-- of the chips' pin ranges overlap, a 'MockFSException' is returned
-- in a 'Left' value.
--
-- >>> import System.GPIO.Monad
-- >>> let mockChip = MockGpioChip "chip0" 0 (replicate 16 defaultMockPinState)
-- >>> fst <$> runSysfsGpioMock pins initialMockWorld [mockChip]
-- Right [Pin 0,Pin 1,Pin 2,Pin 3,Pin 4,Pin 5,Pin 6,Pin 7,Pin 8,Pin 9,Pin 10,Pin 11,Pin 12,Pin 13,Pin 14,Pin 15]
-- >>> fst <$> runSysfsGpioMock (openPin (Pin 32)) initialMockWorld [mockChip]
-- Left InvalidPin (Pin 32)
runSysfsGpioMock :: SysfsGpioMock a -> MockWorld -> [MockGpioChip] -> Either SomeException (a, MockWorld)
runSysfsGpioMock a = runSysfsMock (runSysfsGpioT a)

-- | Like 'runSysfsGpioMock', but returns only the computation's
-- value.
evalSysfsGpioMock :: SysfsGpioMock a -> MockWorld -> [MockGpioChip] -> Either SomeException a
evalSysfsGpioMock a = evalSysfsMock (runSysfsGpioT a)

-- | Like 'runSysfsGpioMock', but returns only the final 'MockWorld'.
execSysfsGpioMock :: SysfsGpioMock a -> MockWorld -> [MockGpioChip] -> Either SomeException MockWorld
execSysfsGpioMock a = execSysfsMock (runSysfsGpioT a)

-- | The simplest possible ('IO'-enabled) mock @sysfs@ monad. Like
-- 'SysfsMock', but allows you to mix 'IO' operations into your
-- @sysfs@ computations, as well.
--
-- NB: this monad /cannot/ run GPIO computations; its only use is to
-- mock @sysfs@ operations on an extremely limited mock @sysfs@
-- simulator.
--
-- You probably do not want to use this monad; see either
-- 'SysfsGpioMock' or 'SysfsGpioMockIO', which adds GPIO computations
-- to this mock @sysfs@ environment.
type SysfsMockIO = SysfsMockT IO

-- | An 'IO' version of 'runSysfsMockT'. Errors are expressed as
-- exceptions.
--
-- >>> let mockChip = MockGpioChip "chip0" 0 (replicate 16 defaultMockPinState)
-- >>> fst <$> runSysfsMockIO (getDirectoryContents "/sys/class/gpio") initialMockWorld [mockChip]
-- ["gpiochip0","export","unexport"]
-- >>> runSysfsMockIO (getDirectoryContents "/sys/class/does_not_exist") initialMockWorld [mockChip]
-- *** Exception: /sys/class/does_not_exist: Mock.Internal.cd: does not exist
runSysfsMockIO :: SysfsMockIO a -> MockWorld -> [MockGpioChip] -> IO (a, MockWorld)
runSysfsMockIO = runSysfsMockT

-- | Like 'runSysfsMockIO', but returns only the computation's value.
evalSysfsMockIO :: SysfsMockIO a -> MockWorld -> [MockGpioChip] -> IO a
evalSysfsMockIO a w chips = fst <$> runSysfsMockIO a w chips

-- | Like 'runSysfsMockIO', but returns only the final 'MockWorld'.
execSysfsMockIO :: SysfsMockIO a -> MockWorld -> [MockGpioChip] -> IO MockWorld
execSysfsMockIO a w chips = snd <$> runSysfsMockIO a w chips

-- | Like 'SysfsGpioMock', but wraps 'IO' so that you can mix 'IO'
-- actions and GPIO actions in a mock GPIO environment.
type SysfsGpioMockIO = SysfsGpioT SysfsMockIO

-- | Run a 'SysfsGpioMockIO' computation with an initial mock world
-- and list of 'MockGpioChip's, and return a tuple containing the
-- computation's value and the final 'MockWorld'.
--
-- Before running the computation, the 'MockWorld' is populated with
-- the GPIO pins as specified by the list of 'MockGpioChip's. If any
-- of the chips' pin ranges overlap, a 'MockFSException' is thrown.
--
-- >>> import System.GPIO.Monad
-- >>> let mockChip = MockGpioChip "chip0" 0 (replicate 16 defaultMockPinState)
-- >>> fst <$> runSysfsGpioMockIO pins initialMockWorld [mockChip]
-- [Pin 0,Pin 1,Pin 2,Pin 3,Pin 4,Pin 5,Pin 6,Pin 7,Pin 8,Pin 9,Pin 10,Pin 11,Pin 12,Pin 13,Pin 14,Pin 15]
-- >>> fst <$> runSysfsGpioMockIO (openPin (Pin 32)) initialMockWorld [mockChip]
-- *** Exception: InvalidPin (Pin 32)
runSysfsGpioMockIO :: SysfsGpioMockIO a -> MockWorld -> [MockGpioChip] -> IO (a, MockWorld)
runSysfsGpioMockIO a = runSysfsMockIO (runSysfsGpioT a)

-- | Like 'runSysfsGpioMockIO', but returns only the computation's
-- value.
evalSysfsGpioMockIO :: SysfsGpioMockIO a -> MockWorld -> [MockGpioChip] -> IO a
evalSysfsGpioMockIO a = evalSysfsMockIO (runSysfsGpioT a)

-- | Like 'runSysfsGpioMockIO', but returns only the final
-- 'MockWorld'.
execSysfsGpioMockIO :: SysfsGpioMockIO a -> MockWorld -> [MockGpioChip] -> IO MockWorld
execSysfsGpioMockIO a = execSysfsMockIO (runSysfsGpioT a)

-- | Exceptions that can be thrown by mock @sysfs@ filesystem
-- operations.
--
-- Note that, as much as is reasonably possible, when an error occurs,
-- the mock filesystem implementation throws the same exception as
-- would occur in an actual @sysfs@ filesystem (i.e., 'IOError's).
-- However, in a few cases, there are exceptions that are specific to
-- the mock @sysfs@ implementation; in these cases, a
-- 'MockFSException' is thrown.
data MockFSException
  = GpioChipOverlap Pin
    -- ^ The user has defined defined at least two 'MockGpioChip's
    -- with the same pin number, which is an invalid condition
  | InternalError Text
    -- ^ An internal error has occurred in the mock @sysfs@
    -- interpreter, something which should "never happen" and should
    -- be reported to the package maintainer.
  deriving (Show,Eq,Typeable)

instance Exception MockFSException where
  toException = gpioExceptionToException
  fromException = gpioExceptionFromException

makeFileSystem :: (MockM m) => [MockGpioChip] -> SysfsMockT m MockFSZipper
makeFileSystem chips =
  do mapM_ makeChip chips
     getZipper

makeChip :: (MockM m) => MockGpioChip -> SysfsMockT m ()
makeChip chip =
  let chipdir = sysfsPath </> ("gpiochip" ++ show (_base chip))
  in
    addPins (_base chip) (_initialPinStates chip) <$> getPins >>= \case
      Left e -> throwM e
      Right newPinState ->
        do putPins newPinState
           mkdir chipdir
           mkfile (chipdir </> "base") (Constant [intToBS $ _base chip])
           mkfile (chipdir </> "ngpio") (Constant [intToBS $ length (_initialPinStates chip)])
           mkfile (chipdir </> "label") (Constant [toS $ _label chip])

addPins :: Int -> [MockPinState] -> MockPins -> Either MockFSException MockPins
addPins base states pm = foldrM addPin pm (zip (map Pin [base..]) states)

addPin :: (Pin, MockPinState) -> MockPins -> Either MockFSException MockPins
addPin (pin, st) pm =
  let insertLookup = Map.insertLookupWithKey (\_ a _ -> a)
  in
    case insertLookup pin st pm of
      (Nothing, newPm) -> Right newPm
      (Just _, _) -> Left $ GpioChipOverlap pin

pushd :: (MockM m) => FilePath -> SysfsMockT m a -> SysfsMockT m a
pushd path action =
  do z <- getZipper
     let restorePath = Internal.pathFromRoot z
     cd path >>= putZipper
     result <- action
     cd restorePath >>= putZipper
     return result

cd :: (MockM m) => FilePath -> SysfsMockT m MockFSZipper
cd name =
  do fsz <- getZipper
     case Internal.cd name fsz of
       Left e -> throwM e
       Right newz -> return newz

mkdir :: (MockM m) => FilePath -> SysfsMockT m ()
mkdir path =
  let (parentName, childName) = splitFileName path
  in
    do parent <- cd parentName
       either throwM putZipper (Internal.mkdir childName parent)

rmdir :: (MockM m) => FilePath -> SysfsMockT m ()
rmdir path =
  let (parentName, childName) = splitFileName path
  in
    do parent <- cd parentName
       either throwM putZipper (Internal.rmdir childName parent)

mkfile :: (MockM m) => FilePath -> FileType -> SysfsMockT m ()
mkfile path filetype =
  let (parentName, childName) = splitFileName path
  in
    do parent <- cd parentName
       either throwM putZipper (Internal.mkfile childName filetype False parent)

-- | Check whether the specified directory exists in the mock
-- filesystem.
doesDirectoryExist :: (MockM m) => FilePath -> SysfsMockT m Bool
doesDirectoryExist path =
  either (const False) (const True) . Internal.cd path <$> getZipper

-- | Check whether the specified file exists in the mock filesystem.
doesFileExist :: (MockM m) => FilePath -> SysfsMockT m Bool
doesFileExist path =
  let (dirPath, fileName) = splitFileName path
  in
    do cwz <- getZipper
       case Internal.cd dirPath cwz of
         Left _ -> return False
         Right z ->
           return $ isJust (findFile fileName (_cwd z))

-- | Get a directory listing for the specified directory in the mock
-- filesystem.
getDirectoryContents :: (MockM m) => FilePath -> SysfsMockT m [FilePath]
getDirectoryContents path =
  do parent <- _cwd <$> cd path
     return $ fmap dirName (subdirs parent) ++ fmap _fileName (files parent)

-- | Read the contents of the specified file in the mock filesystem.
readFile :: (MockM m) => FilePath -> SysfsMockT m ByteString
readFile path =
  fileAt path >>= \case
    Nothing ->
      do isDirectory <- doesDirectoryExist path
         if isDirectory
            then throwM $ mkIOError InappropriateType "Mock.readFile" Nothing (Just path)
            else throwM $ mkIOError NoSuchThing "Mock.readFile" Nothing (Just path)
    Just (Constant contents) -> return $ C8.unlines contents
    Just (Value pin) -> pinValueToBS . logicalValue <$> pinState pin -- Use the logical "value" here!
    Just (ActiveLow pin) -> activeLowToBS . _activeLow <$> pinState pin
    Just (Direction pin) ->
      do visible <- _userVisibleDirection <$> pinState pin
         if visible
            then pinDirectionToBS . _direction <$> pinState pin
            else throwM $
                   InternalError $
                     unwords ["Mock pin", show pin, "has no direction but direction attribute is exported"]
    Just (Edge pin) ->
      _edge <$> pinState pin >>= \case
        Nothing -> throwM $ InternalError $
          unwords ["Mock pin", show pin, "has no edge but edge attribute is exported"]
        Just edge -> return $ sysfsEdgeToBS edge
    Just _ -> throwM $ mkIOError PermissionDenied "Mock.readFile" Nothing (Just path)

-- | Write the contents of the specified file in the mock filesystem.
writeFile :: (MockM m) => FilePath -> ByteString -> SysfsMockT m ()
writeFile path bs =
  -- NB: In some cases, more than one kind of error can occur (e.g.,
  -- when exporting a pin, the pin number may be invalid, or the pin
  -- may already be exported). We try to emulate what a real @sysfs@
  -- filesystem would do, so the order in which error conditions are
  -- checked matters here!
  fileAt path >>= \case
    Nothing ->
      do isDirectory <- doesDirectoryExist path
         if isDirectory
            then throwM $ mkIOError InappropriateType "Mock.writeFile" Nothing (Just path)
            else throwM $ mkIOError NoSuchThing "Mock.writeFile" Nothing (Just path)
    Just Export ->
      case bsToInt bs of
        Just n -> export (Pin n)
        Nothing -> throwM writeError
    Just Unexport ->
      case bsToInt bs of
        Just n -> unexport (Pin n)
        Nothing -> throwM writeError
    Just (ActiveLow pin) ->
      case bsToActiveLow bs of
        Just b -> putPinState pin (\s -> s {_activeLow = b})
        Nothing -> throwM writeError
    Just (Value pin) ->
      _direction <$> pinState pin >>= \case
        Out ->
          case bsToPinValue bs of
            Just v -> putPinState pin (setLogicalValue v)
            Nothing -> throwM writeError
        _ ->
          throwM permissionError
    Just (Edge pin) ->
      do ps <- pinState pin
         case (_edge ps, _direction ps) of
           (Nothing, _) -> throwM $ InternalError $
             unwords ["Mock pin", show pin, "has no edge but edge attribute is exported"]
           (_, Out) -> throwM $ mkIOError InvalidArgument "Mock.writeFile" Nothing (Just path)
           _ -> case bsToSysfsEdge bs of
                  Just edge -> putPinState pin (\s -> s {_edge = Just edge})
                  Nothing -> throwM writeError
    Just (Direction pin) ->
      -- NB: In Linux @sysfs@, writing a pin's @direction@ attribute
      -- with a "high" or "low" value sets the pin's /physical/ signal
      -- level to that state. In other words, the pin's @active_low@
      -- attribute is not considered when setting the pin's signal
      -- level via the @direction@ attribute. We faithfully mimic that
      -- behavior here.
      --
      -- NB: In Linux @sysfs@, if an input pin has been configured to
      -- generate interrupts (i.e., its @edge@ attribute is not
      -- @none@), changing its @direction@ attribute to @out@
      -- generates an I/O error. We emulate that behavior here.
      do ps <- pinState pin
         case (_userVisibleDirection ps, _edge ps, bsToPinDirection bs) of
           (False, _, _) -> throwM $ InternalError $
             unwords ["Mock pin", show pin, "has no direction but direction attribute is exported"]
           (True, _, Nothing) -> throwM writeError
           (True, Nothing, Just (dir, Nothing)) -> putPinState pin (\s -> s {_direction = dir})
           (True, Nothing, Just (dir, Just v)) -> putPinState pin (\s -> s {_direction = dir, _value = v})
           (True, Just None, Just (dir, Nothing)) -> putPinState pin (\s -> s {_direction = dir})
           (True, Just None, Just (dir, Just v)) -> putPinState pin (\s -> s {_direction = dir, _value = v})
           (True, _, Just (In, _)) -> putPinState pin (\s -> s {_direction = In})
           (True, _, Just (Out, _)) -> throwM $ mkIOError HardwareFault "Mock.writeFile" Nothing (Just path)
    Just _ -> throwM permissionError
  where
    writeError :: IOError
    writeError = mkIOError InvalidArgument "Mock.writeFile" Nothing (Just path)

    permissionError :: IOError
    permissionError = mkIOError PermissionDenied "Mock.writeFile" Nothing (Just path)

    export :: (MockM m) => Pin -> SysfsMockT m ()
    export pin =
      Map.lookup pin <$> getPins >>= \case
        Nothing -> throwM $ mkIOError InvalidArgument "Mock.writeFile" Nothing (Just path)
        Just s ->
          do let pindir = pinDirName pin
             -- Already exported?
             doesDirectoryExist pindir >>= \case
               True -> throwM $ mkIOError ResourceBusy "Mock.writeFile" Nothing (Just path)
               False ->
                 do mkdir pindir
                    mkfile (pinActiveLowFileName pin) (ActiveLow pin)
                    mkfile (pinValueFileName pin) (Value pin)
                    when (_userVisibleDirection s) $
                      mkfile (pinDirectionFileName pin) (Direction pin)
                    when (isJust $ _edge s) $
                      mkfile (pinEdgeFileName pin) (Edge pin)

    unexport :: (MockM m) => Pin -> SysfsMockT m ()
    unexport pin =
      do let pindir = pinDirName pin
         doesDirectoryExist pindir >>= \case
           True -> rmdir pindir -- recursive
           False -> throwM $ mkIOError InvalidArgument "Mock.writeFile" Nothing (Just path)

fileAt :: (MockM m) => FilePath -> SysfsMockT m (Maybe FileType)
fileAt path =
  let (dirPath, fileName) = splitFileName path
  in
    findFile fileName . _cwd <$> cd dirPath

-- | For the mock filesystem, this action is equivalent to
-- 'writeFile'.
unlockedWriteFile :: (MockM m) => FilePath -> ByteString -> SysfsMockT m ()
unlockedWriteFile = writeFile

-- | Polling is not implemented for the mock filesystem, so this
-- action always returns the value @1@.
pollFile :: (MockM m) => FilePath -> Int -> SysfsMockT m CInt
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
sysfsRootZipper = MockFSZipper sysfsRoot []
