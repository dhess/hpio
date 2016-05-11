{-|
Module      : System.GPIO.Linux.Sysfs.Mock
Description : A mock MonadSysfs instance.
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

A mock 'M.MonadSysfs' instance, for testing GPIO programs.

Note that this monad only mocks the subset of @sysfs@ functionality
required for GPIO programs. It does not mock the entire @sysfs@
filesystem.

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

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
         -- * Run mock GPIO computations
       , SysfsGpioMock
       , runSysfsGpioMock
       , evalSysfsGpioMock
       , execSysfsGpioMock
         -- * Mock @sysfs@ exceptions.
       , MockFSException(..)
         -- * Run mock @sysfs@ computations.
         --
         -- | Generally speaking, you should not need to use this
         -- type, as it's not very useful on its own. It is primarily
         -- exported for unit testing.
         --
         -- If you want to run mock GPIO computations, use
         -- 'SysfsMockT' for buildling transformer stacks, or
         -- 'SysfsGpioMock' for simple, pure computations.
       , SysfsMock
       , runSysfsMock
       , evalSysfsMock
       , execSysfsMock
         -- * Mock @sysfs@ operations
         --
         -- | Generally speaking, you should not need these functions.
         -- They are primarily exported for unit testing.
       , doesDirectoryExist
       , doesFileExist
       , getDirectoryContents
       , readFile
       , writeFile
       , unlockedWriteFile
       , pollFile
       ) where

#if ! MIN_VERSION_base(4,8,0)
import Prelude.Compat ((<$>), Applicative)
#endif

import Prelude hiding (readFile, writeFile)
import Control.Applicative (Alternative)
import Control.Exception (Exception(..), SomeException)
import Control.Monad (MonadPlus, when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM)
import Control.Monad.Catch.Pure (Catch, runCatch)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State.Strict (MonadState(..), StateT(..), gets, execStateT)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Writer (MonadWriter(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8 (pack, unlines)
import Data.Foldable (foldrM)
import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, insert, insertLookupWithKey, lookup)
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt(..))
import GHC.IO.Exception (IOErrorType(..))
import System.FilePath ((</>), splitFileName)
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
import System.IO.Error (mkIOError)

-- | A mock pin.
--
-- Note that in the real Linux @sysfs@, pins keep their state even
-- after they're unexported.
data MockPinState =
  MockPinState {_direction :: !PinDirection
               ,_userVisibleDirection :: !Bool
               ,_activeLow :: !Bool
               ,_value :: !PinValue -- This is the line level, not the
                                    -- logical level
               ,_edge :: Maybe SysfsEdge}
  deriving (Show,Eq)

-- | Linux @sysfs@ GPIO natively supports active-low logic levels. A
-- pin's "active" level is controlled by the pin's @active_low@
-- attribute. We call the pin's signal level as modulated by its
-- active level the pin's /logical value/. This function returns the
-- mock pin's logical value.
logicalValue :: MockPinState -> PinValue
logicalValue s
  | _activeLow s = invertValue $ _value s
  | otherwise = _value s

-- | This function sets the 'MockPinState' signal level to the given
-- /logical/ value; i.e., the value set takes into consideration the
-- pin's active level.
setLogicalValue :: PinValue -> MockPinState -> MockPinState
setLogicalValue v s
  | _activeLow s = s {_value = invertValue v}
  | otherwise = s {_value = v}

-- | Default initial state of mock pins.
defaultMockPinState :: MockPinState
defaultMockPinState =
  MockPinState {_direction = Out
               ,_userVisibleDirection = True
               ,_activeLow = False
               ,_value = Low
               ,_edge = Just None}

-- | A mock "gpiochip," which, in the Linux @sysfs@ GPIO filesystem,
-- describes the GPIO pin numbers that are available to userspace.
--
-- Note that the '_initialPinStates' list is only used to construct a
-- mock filesystem. For each 'MockPinState' value in the list, a mock
-- pin is created in the mock filesystem with the path
-- @\/sys\/class\/gpio\/gpioN@, where @N@ is @_base@ + the pin's index in
-- the '_initialPinStates' list.
data MockGpioChip =
  MockGpioChip {_label :: !String
               ,_base :: !Int
               ,_initialPinStates :: [MockPinState]}
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
-- To preserve the illusion of an actual @sysfs@ GPIO filesystem,
-- then, the 'MockWorld' type is opaque and can only be manipulated
-- via the handful of operations that are implemented in this module,
-- which have been designed to keep the internal state of the mock
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
newtype SysfsMockT m a =
  SysfsMockT {unSysfsMockT :: StateT MockWorld m a}
  deriving (Functor,Alternative,Applicative,Monad,MonadFix,MonadPlus,MonadThrow,MonadCatch,MonadMask,MonadCont,MonadIO,MonadReader r,MonadError e,MonadWriter w,MonadState MockWorld,MonadTrans)

getZipper :: (Monad m) => SysfsMockT m MockFSZipper
getZipper = gets _zipper

putZipper :: (Monad m) => MockFSZipper -> SysfsMockT m ()
putZipper z =
  do s <- get
     put $ s {_zipper = z}

getPins :: (Monad m) => SysfsMockT m MockPins
getPins = gets _pins

pinState :: (Functor m, MonadThrow m) => Pin -> SysfsMockT m MockPinState
pinState pin =
  Map.lookup pin <$> getPins >>= \case
    Nothing -> throwM $ InternalError ("An operation attempted to get the mock pin state for non-existent pin " ++ show pin)
    Just s -> return s

putPins :: (Monad m) => MockPins -> SysfsMockT m ()
putPins ps =
  do s <- get
     put $ s {_pins = ps}

putPinState :: (Functor m, MonadThrow m) => Pin -> (MockPinState -> MockPinState) -> SysfsMockT m ()
putPinState pin f =
  do ps <- pinState pin
     (Map.insert pin (f ps) <$> getPins) >>= putPins

-- | Run a mock @sysfs@ computation in monad 'm' with an initial mock
-- world and list of 'MockGpioChip'; and return a tuple containing the
-- computation's value and the final 'MockWorld'. If an exception
-- occurs in the mock computation, a 'MockFSException' is thrown.
--
-- Before running the computation, the mock filesystem is populated
-- with the GPIO pins as specified by the list of 'MockGpioChip's. If
-- any of the chips in the list are already present in the filesystem,
-- or if any of the chips' pin ranges overlap, a 'MockFSException' is
-- thrown.
--
-- Typically, you will only need this action if you're trying to mock
-- Linux @sysfs@ GPIO computations using a custom monad transformer
-- stack. For simple cases, see 'runSysfsGpioMock'.
runSysfsMockT :: (Functor m, MonadThrow m) => SysfsMockT m a -> MockWorld -> [MockGpioChip] -> m (a, MockWorld)
runSysfsMockT action world chips =
  do startState <- execStateT (unSysfsMockT $ pushd "/" (makeFileSystem chips)) world
     runStateT (unSysfsMockT action) startState

instance (Functor m, MonadThrow m) => M.MonadSysfs (SysfsMockT m) where
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
-- You probably do not want to use this monad; see 'SysfsGpioMock',
-- which adds mock GPIO computations to this mock @sysfs@ environment.
type SysfsMock = SysfsMockT Catch

-- | A pure version of 'runSysfsMockT' which returns errors in a
-- 'Left', and both the computation result and the final state of the
-- 'MockWorld' in a 'Right'.
runSysfsMock :: SysfsMock a -> MockWorld -> [MockGpioChip] -> Either SomeException (a, MockWorld)
runSysfsMock a w chips = runCatch $ runSysfsMockT a w chips

-- | Like 'runSysfsMock', but returns only the computation result in a
-- 'Right'.
evalSysfsMock :: SysfsMock a -> MockWorld -> [MockGpioChip] -> Either SomeException a
evalSysfsMock a w chips = fst <$> runSysfsMock a w chips

-- | Like 'runSysfsMock', but returns only the final 'MockWorld' state
-- in a 'Right'.
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
-- Before running the computation, the mock filesystem is populated
-- with the GPIO pins as specified by the list of 'MockGpioChip's. If
-- any of the chips in the list are already present in the filesystem,
-- or if any of the chips' pin ranges overlap, an error is returned.
runSysfsGpioMock :: SysfsGpioMock a -> MockWorld -> [MockGpioChip] -> Either SomeException (a, MockWorld)
runSysfsGpioMock a = runSysfsMock (runSysfsGpioT a)

-- | Run a 'SysfsGpioMock' computation with an initial mock world and
-- list of 'MockGpioChip's, and return the computation's value,
-- discarding the final state. Any exceptions that occur in the mock
-- computation are returned as a 'Left' value.
--
-- Before running the computation, the mock filesystem is populated
-- with the GPIO pins as specified by the list of 'MockGpioChip's. If
-- any of the chips in the list are already present in the filesystem,
-- or if any of the chips' pin ranges overlap, an error is returned.
evalSysfsGpioMock :: SysfsGpioMock a -> MockWorld -> [MockGpioChip] -> Either SomeException a
evalSysfsGpioMock a = evalSysfsMock (runSysfsGpioT a)

-- | Run a 'SysfsGpioMock' computation with an initial mock world and
-- list of 'MockGpioChip's, and return the final 'MockWorld',
-- discarding the computation's value. Any exceptions that occur in
-- the mock computation are returned as a 'Left' value.
--
-- Before running the computation, the mock filesystem is populated
-- with the GPIO pins as specified by the list of 'MockGpioChip's. If
-- any of the chips in the list are already present in the filesystem,
-- or if any of the chips' pin ranges overlap, an error is returned.
execSysfsGpioMock :: SysfsGpioMock a -> MockWorld -> [MockGpioChip] -> Either SomeException MockWorld
execSysfsGpioMock a = execSysfsMock (runSysfsGpioT a)

-- | Exceptions that can be thrown by mock @sysfs@ filesystem
-- operations.
--
-- Note that, as much as is reasonably possible, when an error occurs,
-- the mock filesystem implementation throws the same exception as
-- would occur in an actual @sysfs@ filesystem, so callers should
-- expect to encounter 'IOError's when errors occur in the mock
-- filesystem. However, in a few cases, there are exceptions that are
-- specific to the mock @sysfs@ implementation; in these cases, a
-- 'MockFSException' is thrown.
data MockFSException
  = GpioChipOverlap Pin
    -- ^ The user has defined defined at least two 'MockGpioChip's
    -- with the same pin number, which is an invalid condition
  | InternalError String
    -- ^ An internal error has occurred in the mock @sysfs@
    -- interpreter, something which should "never happen" and should
    -- be reported to the package maintainer.
  deriving (Show,Eq,Typeable)

instance Exception MockFSException where
  toException = gpioExceptionToException
  fromException = gpioExceptionFromException

makeFileSystem :: (Functor m, MonadThrow m) => [MockGpioChip] -> SysfsMockT m MockFSZipper
makeFileSystem chips =
  do mapM_ makeChip chips
     getZipper

makeChip :: (Functor m, MonadThrow m) => MockGpioChip -> SysfsMockT m ()
makeChip chip =
  let chipdir = sysfsPath </> ("gpiochip" ++ show (_base chip))
  in
    addPins (_base chip) (_initialPinStates chip) <$> getPins >>= \case
      Left e -> throwM e
      Right newPinState ->
        do putPins newPinState
           mkdir chipdir
           mkfile (chipdir </> "base") (Const [intToBS $ _base chip])
           mkfile (chipdir </> "ngpio") (Const [intToBS $ length (_initialPinStates chip)])
           mkfile (chipdir </> "label") (Const [C8.pack $ _label chip])

addPins :: Int -> [MockPinState] -> MockPins -> Either MockFSException MockPins
addPins base states pm = foldrM addPin pm (zip (map Pin [base..]) states)

addPin :: (Pin, MockPinState) -> MockPins -> Either MockFSException MockPins
addPin (pin, st) pm =
  let insertLookup = Map.insertLookupWithKey (\_ a _ -> a)
  in
    case insertLookup pin st pm of
      (Nothing, newPm) -> Right newPm
      (Just _, _) -> Left $ GpioChipOverlap pin

pushd :: (MonadThrow m) => FilePath -> SysfsMockT m a -> SysfsMockT m a
pushd path action =
  do z <- getZipper
     let restorePath = Internal.pathFromRoot z
     cd path >>= putZipper
     result <- action
     cd restorePath >>= putZipper
     return result

cd :: (MonadThrow m) => FilePath -> SysfsMockT m MockFSZipper
cd name =
  do fsz <- getZipper
     case Internal.cd name fsz of
       Left e -> throwM e
       Right newz -> return newz

mkdir :: (MonadThrow m) => FilePath -> SysfsMockT m ()
mkdir path =
  let (parentName, childName) = splitFileName path
  in
    do parent <- cd parentName
       either throwM putZipper (Internal.mkdir childName parent)

rmdir :: (MonadThrow m) => FilePath -> SysfsMockT m ()
rmdir path =
  let (parentName, childName) = splitFileName path
  in
    do parent <- cd parentName
       either throwM putZipper (Internal.rmdir childName parent)

mkfile :: (MonadThrow m) => FilePath -> FileType -> SysfsMockT m ()
mkfile path filetype =
  let (parentName, childName) = splitFileName path
  in
    do parent <- cd parentName
       either throwM putZipper (Internal.mkfile childName filetype False parent)

-- | Check whether the specified directory exists in the mock
-- filesystem.
doesDirectoryExist :: (Monad m) => FilePath -> SysfsMockT m Bool
doesDirectoryExist path =
  do cwz <- getZipper
     return $ either (const False) (const True) (Internal.cd path cwz)

-- | Check whether the specified file exists in the mock filesystem.
doesFileExist :: (Monad m) => FilePath -> SysfsMockT m Bool
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
getDirectoryContents :: (Functor m, MonadThrow m) => FilePath -> SysfsMockT m [FilePath]
getDirectoryContents path =
  do parent <- _cwd <$> cd path
     return $ fmap dirName (subdirs parent) ++ fmap _fileName (files parent)

-- | Read the contents of the specified file in the mock filesystem.
readFile :: (Functor m, MonadThrow m) => FilePath -> SysfsMockT m ByteString
readFile path =
  fileAt path >>= \case
    Nothing ->
      do isDirectory <- doesDirectoryExist path
         if isDirectory
            then throwM $ mkIOError InappropriateType "Mock.readFile" Nothing (Just path)
            else throwM $ mkIOError NoSuchThing "Mock.readFile" Nothing (Just path)
    Just (Const contents) -> return $ C8.unlines contents
    Just (Value pin) -> pinValueToBS . logicalValue <$> pinState pin -- Use the logical "value" here!
    Just (ActiveLow pin) -> activeLowToBS . _activeLow <$> pinState pin
    Just (Direction pin) ->
      do visible <- _userVisibleDirection <$> pinState pin
         if visible
            then do direction <- _direction <$> pinState pin
                    return $ pinDirectionToBS direction
            else throwM $ InternalError ("Mock pin " ++ show pin ++ " has no direction but direction attribute is exported")
    Just (Edge pin) ->
      _edge <$> pinState pin >>= \case
        Nothing -> throwM $ InternalError ("Mock pin " ++ show pin ++ " has no edge but edge attribute is exported")
        Just edge -> return $ sysfsEdgeToBS edge
    Just _ -> throwM $ mkIOError PermissionDenied "Mock.readFile" Nothing (Just path)

-- | Write the contents of the specified file in the mock filesystem.
writeFile :: (Functor m, MonadThrow m) => FilePath -> ByteString -> SysfsMockT m ()
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
           (Nothing, _) -> throwM $ InternalError ("Mock pin " ++ show pin ++ " has no edge but edge attribute is exported")
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
           (False, _, _) -> throwM $ InternalError ("Mock pin " ++ show pin ++ " has no direction but direction attribute is exported")
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

    export :: (Functor m, MonadThrow m) => Pin -> SysfsMockT m ()
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

    unexport :: (MonadThrow m) => Pin -> SysfsMockT m ()
    unexport pin =
      do let pindir = pinDirName pin
         doesDirectoryExist pindir >>= \case
           True -> rmdir pindir -- recursive
           False -> throwM $ mkIOError InvalidArgument "Mock.writeFile" Nothing (Just path)

fileAt :: (Functor m, MonadThrow m) => FilePath -> SysfsMockT m (Maybe FileType)
fileAt path =
  let (dirPath, fileName) = splitFileName path
  in
    do parent <- _cwd <$> cd dirPath
       return $ findFile fileName parent

-- | For the mock filesystem, this action is equivalent to
-- 'writeFile'.
unlockedWriteFile :: (Functor m, MonadThrow m) => FilePath -> ByteString -> SysfsMockT m ()
unlockedWriteFile = writeFile

-- | Polling is not implemented for the mock filesystem, so this
-- action always returns the value @1@.
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
sysfsRootZipper = MockFSZipper sysfsRoot []
