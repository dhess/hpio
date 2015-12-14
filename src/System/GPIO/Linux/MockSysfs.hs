-- | A "mock" 'MonadSysfs' instance for testing
-- "System.GPIO.Free.GpioF" programs in a faux-Linux environment.
--
-- The 'MockSysfsT' transformer makes a best-effort attempt to emulate
-- an actual Linux sysfs GPIO environment, including but not limited
-- to throwing the same 'IOError's that the default
-- 'System.GPIO.Linux.Sysfs.MonadSysfs' implementation throws in the
-- presence of program errors.
--
-- == TODO
--
-- The following exceptional states cannot yet be emulated:
--
-- * sysfs is not present
--
-- * Insufficient permissions.
--
-- * While the program is running, a pin is exported/unexported by a
-- third party, or its direction changed.
--
-- * Generally broken sysfs (e.g., files which are expected to be
-- present during standard operation are not).

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module System.GPIO.Linux.MockSysfs
       ( -- * The 'MockSysfs' monad
         MockSysfsT
       , MockSysfs
       , runMockSysfsT
       , evalMockSysfsT
       , execMockSysfsT
       , runMockSysfs
       , execMockSysfs
       , evalMockSysfs
         -- * 'MockSysfs' types
       , MockState(..)
       , MockStateMap
       , MockWorld(..)
       , defaultState
       ) where

import Control.Conditional (ifM, whenM, unlessM)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.RWS.Strict (RWST, gets, modify, put, runRWST, evalRWST, execRWST)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.IO.Exception (IOErrorType(..))
import System.GPIO.Free (PinDirection(..), Pin(..), PinValue(..))
import System.GPIO.Linux.MonadSysfs
import System.IO.Error (mkIOError, ioeSetErrorString)

-- | Keep track of the state of mock pins. In real Linux 'sysfs', pins
-- state even after they're unexported.
data MockState =
  MockState { hasUserDirection :: !Bool -- is direction visible to the user?
            , direction :: !PinDirection
            , value :: !PinValue
            } deriving (Show, Eq)

-- | Maps pins to their state
type MockStateMap = Map Pin MockState

-- | The 'MockWorld' keeps track of exported/unexported status.
data MockWorld =
  MockWorld { unexported :: MockStateMap
            , exported :: MockStateMap }

-- | Default initial state of mock pins.
defaultState :: MockState
defaultState = MockState True Out Low

-- | A monad transformer which adds mock sysfs computations to an
-- inner monad 'm'.
type MockSysfsT m = RWST () [Text] MockWorld m

-- | Run a mock sysfs computation with the given 'MockWorld', and
-- return a tuple containing the final value, log, and final
-- 'MockWorld' state.
runMockSysfsT :: (MonadIO m) => MockSysfsT m a -> MockWorld -> m (a, MockWorld, [Text])
runMockSysfsT action world = runRWST action () world

-- | Run a mock sysfs computation with the given 'MockWorld, and
-- return a tuple containing the final value and log, discarding the
-- state.
evalMockSysfsT :: (MonadIO m) => MockSysfsT m a -> MockWorld -> m (a, [Text])
evalMockSysfsT action world = evalRWST action () world

-- | Run a mock sysfs computation with the given 'MockWorld, and
-- return a tuple containing the final state and log, discarding the
-- value.
execMockSysfsT :: (MonadIO m) => MockSysfsT m a -> MockWorld -> m (MockWorld, [Text])
execMockSysfsT action world = execRWST action () world

-- | The base mock sysfs monad, which sits on top of 'IO'.
type MockSysfs a = RWST () [Text] MockWorld IO a

-- | Run a 'MockSysfs' computation with the given 'MockWorld', and
-- return a tuple containing the final value, log, and final
-- 'MockWorld' state.
runMockSysfs :: (MonadIO m) => MockSysfsT m a -> MockWorld -> m (a, MockWorld, [Text])
runMockSysfs = runMockSysfsT

-- | Run a 'MockSysfs' computation with the given 'MockWorld, and
-- return a tuple containing the final value and log, discarding the
-- state.
evalMockSysfs :: (MonadIO m) => MockSysfsT m a -> MockWorld -> m (a, [Text])
evalMockSysfs = evalMockSysfsT

-- | Run a 'MockSysfs' computation with the given 'MockWorld, and
-- return a tuple containing the final state and log, discarding the
-- value.
execMockSysfs :: (MonadIO m) => MockSysfsT m a -> MockWorld -> m (MockWorld, [Text])
execMockSysfs = execMockSysfsT

instance (MonadIO m) => MonadSysfs (MockSysfsT m) where
  sysfsIsPresent = sysfsIsPresentMock
  pinIsExported = pinIsExportedMock
  pinHasDirection = pinHasDirectionMock
  exportPin = exportPinMock
  unexportPin = unexportPinMock
  readPinDirection = readPinDirectionMock
  writePinDirection = writePinDirectionMock
  writePinDirectionWithValue = writePinDirectionWithValueMock
  readPinValue = readPinValueMock
  writePinValue = writePinValueMock
  availablePins = availablePinsMock

sysfsIsPresentMock :: (Monad m) => MockSysfsT m Bool
sysfsIsPresentMock = return True

pinIsExportedMock :: (Monad m) => Pin -> MockSysfsT m Bool
pinIsExportedMock p = exportedPinState p >>= return . isJust

-- The way this is implemented in 'MonadSysfs' is, if the pin is not
-- exported, or if the pin does not have a user-visible direction
-- attribute, it's 'False', otherwise 'True'.
pinHasDirectionMock :: (Monad m) => Pin -> MockSysfsT m Bool
pinHasDirectionMock p = userVisibleDirection p >>= return . isJust

exportPinMock :: (MonadIO m) => Pin -> MockSysfsT m ()
exportPinMock p =
  do whenM (pinIsExportedMock p) $
       liftIO $ ioError exportErrorResourceBusy
     ifM (pinIsUnexported p)
       (do unexportedPins <- gets unexported
           exportedPins <- gets exported
           let (newUnexp, newExp) = movePin p unexportedPins exportedPins
           put $ MockWorld newUnexp newExp)
       (liftIO $ ioError exportErrorInvalidArgument)

unexportPinMock :: (MonadIO m) => Pin -> MockSysfsT m ()
unexportPinMock p =
  ifM (pinIsExportedMock p)
    (liftIO $ ioError unexportErrorInvalidArgument)
    (do exportedPins <- gets exported
        unexportedPins <- gets unexported
        let (newExp, newUnexp) = movePin p exportedPins unexportedPins
        put $ MockWorld newUnexp newExp)

readPinDirectionMock :: (MonadIO m) => Pin -> MockSysfsT m String
readPinDirectionMock p =
  userVisibleDirection p >>= \case
    Nothing -> liftIO $ ioError (readPinDirectionErrorNoSuchThing p)
    Just In -> return "in\n"
    Just Out -> return "out\n"

writePinDirectionMock :: (MonadIO m) => Pin -> PinDirection -> MockSysfsT m ()
writePinDirectionMock p dir =
  guardedPinState p id hasUserDirection >>= \case
    Nothing -> liftIO $ ioError (writePinDirectionErrorNoSuchThing p)
    Just state -> modifyExportedPinState p (state { direction = dir })

writePinDirectionWithValueMock :: (MonadIO m) => Pin -> PinValue -> MockSysfsT m ()
writePinDirectionWithValueMock p v =
  guardedPinState p id hasUserDirection >>= \case
    Nothing -> liftIO $ ioError (writePinDirectionErrorNoSuchThing p)
    Just state -> modifyExportedPinState p (state { direction = Out, value = v})

readPinValueMock :: (MonadIO m) => Pin -> MockSysfsT m String
readPinValueMock p =
  checkedPinState p value (readPinValueErrorNoSuchThing p) >>= \case
    Low -> return "0\n"
    High -> return "1\n"

writePinValueMock :: (MonadIO m) => Pin -> PinValue -> MockSysfsT m ()
writePinValueMock p v =
  do state <- checkedPinState p id (writePinValueErrorNoSuchThing p)
     case (direction state) of
       In -> liftIO $ ioError (writePinValueErrorPermissionDenied p)
       Out -> modifyExportedPinState p (state { value = v })

-- XXX TODO: this function doesn't have any way to emulate the failure
-- of the hunt for the "base" and "ngpio" files in GPIO chip
-- subdirectories. Under normal conditions, those files will always
-- exist, but....
availablePinsMock :: (MonadIO m) => MockSysfsT m [Pin]
availablePinsMock =
  do unlessM sysfsIsPresentMock $
       liftIO $ ioError availablePinsErrorNoSuchThing
     exportedPins <- gets (Map.keys . exported)
     unexportedPins <- gets (Map.keys . unexported)
     return $ sort $ exportedPins ++ unexportedPins


-- Convenience functions
--

pinIsUnexported :: (Monad m) => Pin -> MockSysfsT m Bool
pinIsUnexported p = unexportedPinState p >>= return . isJust

-- Note: if 'p' is not in 'src' then 'src' and 'dst' will be unchanged.
movePin :: Pin -> MockStateMap -> MockStateMap -> (MockStateMap, MockStateMap)
movePin p src dst =
  case (Map.lookup p src) of
    Nothing -> (src, dst)
    Just state -> (Map.delete p src, Map.insert p state dst)

modifyExportedPinState :: (Monad m) => Pin -> MockState -> MockSysfsT m ()
modifyExportedPinState p newState = modify $ \world ->
  let exportedPins = exported world
      newExported = Map.insert p newState exportedPins
  in world { exported = newExported }

checkedPinState :: (MonadIO m) => Pin -> (MockState -> a) -> IOError -> MockSysfsT m a
checkedPinState p f ioe =
  exportedPinState p >>= \case
    Nothing -> liftIO $ ioError ioe
    Just state -> return $ f state

guardedPinState :: (Monad m) => Pin -> (MockState -> a) -> (MockState -> Bool) -> MockSysfsT m (Maybe a)
guardedPinState p f predicate =
  do exportedPins <- gets exported
     return $ guardedPinState' p exportedPins f predicate

guardedPinState' :: Pin -> MockStateMap -> (MockState -> a) -> (MockState -> Bool) -> Maybe a
guardedPinState' p stateMap f predicate =
  do pinState <- Map.lookup p stateMap
     guard $ predicate pinState
     return $ f pinState

userVisibleDirection :: (Monad m) => Pin -> MockSysfsT m (Maybe PinDirection)
userVisibleDirection p = guardedPinState p direction hasUserDirection

exportedPinState :: (Monad m) => Pin -> MockSysfsT m (Maybe MockState)
exportedPinState p =
  do exportedPins <- gets exported
     return $ Map.lookup p exportedPins

unexportedPinState :: (Monad m) => Pin -> MockSysfsT m (Maybe MockState)
unexportedPinState p =
  do unexportedPins <- gets unexported
     return $ Map.lookup p unexportedPins


-- Mock 'IOError's. We make these as close to the real thing as is
-- reasonble.
--

-- Pin cannot be exported.
exportErrorInvalidArgument :: IOError
exportErrorInvalidArgument = mkErrorInvalidArgument "hClose" exportFileName

-- Pin already exported.
exportErrorResourceBusy :: IOError
exportErrorResourceBusy = mkErrorResourceBusy "hClose" exportFileName

-- Pin cannot be unexported.
unexportErrorInvalidArgument :: IOError
unexportErrorInvalidArgument = mkErrorInvalidArgument "hClose" unexportFileName

-- Direction file doesn't exist.
readPinDirectionErrorNoSuchThing :: Pin -> IOError
readPinDirectionErrorNoSuchThing p = mkErrorNoSuchThing "openFile" (pinDirectionFileName p)

writePinDirectionErrorNoSuchThing :: Pin -> IOError
writePinDirectionErrorNoSuchThing = readPinDirectionErrorNoSuchThing

-- Value file doesn't exist.
readPinValueErrorNoSuchThing :: Pin -> IOError
readPinValueErrorNoSuchThing p = mkErrorNoSuchThing "openFile" (pinValueFileName p)

writePinValueErrorNoSuchThing :: Pin -> IOError
writePinValueErrorNoSuchThing = readPinValueErrorNoSuchThing

-- Value file can't be written.
writePinValueErrorPermissionDenied :: Pin -> IOError
writePinValueErrorPermissionDenied p = mkErrorPermissionDenied "hClose" (pinValueFileName p)

-- GPIO sysfs directory doesn't exist.
availablePinsErrorNoSuchThing :: IOError
availablePinsErrorNoSuchThing = mkErrorNoSuchThing "getDirectoryContents" sysfsPath

-- IOError Helpers
--

mkErrorInvalidArgument :: String -> FilePath -> IOError
mkErrorInvalidArgument location path =
  let err = mkIOError InvalidArgument location Nothing (Just path)
  in ioeSetErrorString err "Invalid argument"

mkErrorResourceBusy :: String -> FilePath -> IOError
mkErrorResourceBusy location path =
  let err = mkIOError ResourceBusy location Nothing (Just path)
  in ioeSetErrorString err "Device or resource busy"

mkErrorNoSuchThing :: String -> FilePath -> IOError
mkErrorNoSuchThing location path =
  let err = mkIOError NoSuchThing location Nothing (Just path)
  in ioeSetErrorString err "No such file or directory"

mkErrorPermissionDenied :: String -> FilePath -> IOError
mkErrorPermissionDenied location path =
  let err = mkIOError PermissionDenied location Nothing (Just path)
  in ioeSetErrorString err "Operation not permitted"
