-- | A "mock" 'MonadSysfs' instance for testing
-- "System.GPIO.Free.GpioF" programs in a faux-Linux environment.
--
-- The 'SysfsMockT' transformer makes a best-effort attempt to emulate
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module System.GPIO.Linux.SysfsMock
       ( -- * The 'SysfsMock' monad
         SysfsMockT(..)
       , SysfsMock
       , runSysfsMockT
       , evalSysfsMockT
       , execSysfsMockT
       , runSysfsMock
       , execSysfsMock
       , evalSysfsMock
         -- * 'SysfsMock' types
       , MockState(..)
       , MockStateMap
       , MockWorld(..)
       , defaultState
       ) where

import Control.Applicative
import Control.Conditional (ifM, whenM, unlessM)
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.RWS.Strict
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
            , exported :: MockStateMap } deriving (Show, Eq)

-- | Default initial state of mock pins.
defaultState :: MockState
defaultState = MockState True Out Low

-- | A monad transformer which adds mock sysfs computations to an
-- inner monad 'm'.
newtype SysfsMockT m a =
  SysfsMockT { unSysfsMockT :: RWST () [Text] MockWorld m a }
  deriving (Alternative,Applicative,Functor,Monad,MonadRWS () [Text] MockWorld,MonadWriter [Text],MonadState MockWorld,MonadReader (),MonadFix,MonadPlus,MonadIO,MonadCont)

-- | Run a mock sysfs computation with the given 'MockWorld', and
-- return a tuple containing the final value, log, and final
-- 'MockWorld' state.
runSysfsMockT :: (MonadIO m) => SysfsMockT m a -> MockWorld -> m (a, MockWorld, [Text])
runSysfsMockT action world = runRWST (unSysfsMockT action) () world

-- | Run a mock sysfs computation with the given 'MockWorld, and
-- return a tuple containing the final value and log, discarding the
-- state.
evalSysfsMockT :: (MonadIO m) => SysfsMockT m a -> MockWorld -> m (a, [Text])
evalSysfsMockT action world = evalRWST (unSysfsMockT action) () world

-- | Run a mock sysfs computation with the given 'MockWorld, and
-- return a tuple containing the final state and log, discarding the
-- value.
execSysfsMockT :: (MonadIO m) => SysfsMockT m a -> MockWorld -> m (MockWorld, [Text])
execSysfsMockT action world = execRWST (unSysfsMockT action) () world

-- | The base mock sysfs monad, which sits on top of 'IO'.
type SysfsMock a = SysfsMockT IO a

-- | Run a 'SysfsMock' computation with the given 'MockWorld', and
-- return a tuple containing the final value, log, and final
-- 'MockWorld' state.
runSysfsMock :: SysfsMock a -> MockWorld -> IO (a, MockWorld, [Text])
runSysfsMock = runSysfsMockT

-- | Run a 'SysfsMock' computation with the given 'MockWorld, and
-- return a tuple containing the final value and log, discarding the
-- state.
evalSysfsMock :: SysfsMock a -> MockWorld -> IO (a, [Text])
evalSysfsMock = evalSysfsMockT

-- | Run a 'SysfsMock' computation with the given 'MockWorld, and
-- return a tuple containing the final state and log, discarding the
-- value.
execSysfsMock :: SysfsMock a -> MockWorld -> IO (MockWorld, [Text])
execSysfsMock = execSysfsMockT

instance (MonadIO m) => MonadSysfs (SysfsMockT m) where
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

sysfsIsPresentMock :: (Monad m) => SysfsMockT m Bool
sysfsIsPresentMock = return True

pinIsExportedMock :: (Monad m) => Pin -> SysfsMockT m Bool
pinIsExportedMock p = exportedPinState p >>= return . isJust

-- The way this is implemented in 'MonadSysfs' is, if the pin is not
-- exported, or if the pin does not have a user-visible direction
-- attribute, it's 'False', otherwise 'True'.
pinHasDirectionMock :: (Monad m) => Pin -> SysfsMockT m Bool
pinHasDirectionMock p = userVisibleDirection p >>= return . isJust

exportPinMock :: (MonadIO m) => Pin -> SysfsMockT m ()
exportPinMock p =
  do whenM (pinIsExportedMock p) $
       liftIO $ ioError exportErrorResourceBusy
     ifM (pinIsUnexported p)
       (do unexportedPins <- gets unexported
           exportedPins <- gets exported
           let (newUnexp, newExp) = movePin p unexportedPins exportedPins
           put $ MockWorld newUnexp newExp)
       (liftIO $ ioError exportErrorInvalidArgument)

unexportPinMock :: (MonadIO m) => Pin -> SysfsMockT m ()
unexportPinMock p =
  ifM (pinIsExportedMock p)
    (liftIO $ ioError unexportErrorInvalidArgument)
    (do exportedPins <- gets exported
        unexportedPins <- gets unexported
        let (newExp, newUnexp) = movePin p exportedPins unexportedPins
        put $ MockWorld newUnexp newExp)

readPinDirectionMock :: (MonadIO m) => Pin -> SysfsMockT m String
readPinDirectionMock p =
  userVisibleDirection p >>= \case
    Nothing -> liftIO $ ioError (readPinDirectionErrorNoSuchThing p)
    Just In -> return "in\n"
    Just Out -> return "out\n"

writePinDirectionMock :: (MonadIO m) => Pin -> PinDirection -> SysfsMockT m ()
writePinDirectionMock p dir =
  guardedPinState p id hasUserDirection >>= \case
    Nothing -> liftIO $ ioError (writePinDirectionErrorNoSuchThing p)
    Just s -> modifyExportedPinState p (s { direction = dir })

writePinDirectionWithValueMock :: (MonadIO m) => Pin -> PinValue -> SysfsMockT m ()
writePinDirectionWithValueMock p v =
  guardedPinState p id hasUserDirection >>= \case
    Nothing -> liftIO $ ioError (writePinDirectionErrorNoSuchThing p)
    Just s -> modifyExportedPinState p (s { direction = Out, value = v})

readPinValueMock :: (MonadIO m) => Pin -> SysfsMockT m String
readPinValueMock p =
  checkedPinState p value (readPinValueErrorNoSuchThing p) >>= \case
    Low -> return "0\n"
    High -> return "1\n"

writePinValueMock :: (MonadIO m) => Pin -> PinValue -> SysfsMockT m ()
writePinValueMock p v =
  do s <- checkedPinState p id (writePinValueErrorNoSuchThing p)
     case (direction s) of
       In -> liftIO $ ioError (writePinValueErrorPermissionDenied p)
       Out -> modifyExportedPinState p (s { value = v })

-- XXX TODO: this function doesn't have any way to emulate the failure
-- of the hunt for the "base" and "ngpio" files in GPIO chip
-- subdirectories. Under normal conditions, those files will always
-- exist, but....
availablePinsMock :: (MonadIO m) => SysfsMockT m [Pin]
availablePinsMock =
  do unlessM sysfsIsPresentMock $
       liftIO $ ioError availablePinsErrorNoSuchThing
     exportedPins <- gets (Map.keys . exported)
     unexportedPins <- gets (Map.keys . unexported)
     return $ sort $ exportedPins ++ unexportedPins


-- Convenience functions
--

pinIsUnexported :: (Monad m) => Pin -> SysfsMockT m Bool
pinIsUnexported p = unexportedPinState p >>= return . isJust

-- Note: if 'p' is not in 'src' then 'src' and 'dst' will be unchanged.
movePin :: Pin -> MockStateMap -> MockStateMap -> (MockStateMap, MockStateMap)
movePin p src dst =
  case (Map.lookup p src) of
    Nothing -> (src, dst)
    Just s -> (Map.delete p src, Map.insert p s dst)

modifyExportedPinState :: (Monad m) => Pin -> MockState -> SysfsMockT m ()
modifyExportedPinState p newState = modify $ \world ->
  let exportedPins = exported world
      newExported = Map.insert p newState exportedPins
  in world { exported = newExported }

checkedPinState :: (MonadIO m) => Pin -> (MockState -> a) -> IOError -> SysfsMockT m a
checkedPinState p f ioe =
  exportedPinState p >>= \case
    Nothing -> liftIO $ ioError ioe
    Just s -> return $ f s

guardedPinState :: (Monad m) => Pin -> (MockState -> a) -> (MockState -> Bool) -> SysfsMockT m (Maybe a)
guardedPinState p f predicate =
  do exportedPins <- gets exported
     return $ guardedPinState' p exportedPins f predicate

guardedPinState' :: Pin -> MockStateMap -> (MockState -> a) -> (MockState -> Bool) -> Maybe a
guardedPinState' p stateMap f predicate =
  do pinState <- Map.lookup p stateMap
     guard $ predicate pinState
     return $ f pinState

userVisibleDirection :: (Monad m) => Pin -> SysfsMockT m (Maybe PinDirection)
userVisibleDirection p = guardedPinState p direction hasUserDirection

exportedPinState :: (Monad m) => Pin -> SysfsMockT m (Maybe MockState)
exportedPinState p =
  do exportedPins <- gets exported
     return $ Map.lookup p exportedPins

unexportedPinState :: (Monad m) => Pin -> SysfsMockT m (Maybe MockState)
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
