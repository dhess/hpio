-- | A "mock" 'MonadSysfs' instance for testing
-- 'System.GPIO.Free.GpioF' programs in a faux-Linux environment.
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
       ( -- * The SysfsMock monad
         SysfsMockT(..)
       , SysfsMock
       , runSysfsMockT
       , runSysfsMock
       , runSysfsMock'
       , runSysfsMockSafe
         -- * SysfsMock types
       , MockState(..)
       , defaultState
       , MockWorld
       , mockWorld
       ) where

import Control.Applicative
import Control.Error.Script (scriptIO)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import GHC.IO.Exception (IOErrorType(..))
import System.GPIO.Linux.MonadSysfs
import System.GPIO.Linux.Sysfs
import System.GPIO.Types
import System.IO.Error (mkIOError, ioeSetErrorString)

-- | Keep track of the state of mock pins. In real Linux 'sysfs', pins
-- keep their state even after they're unexported.
data MockState =
  MockState { exported :: !Bool
            , hasUserDirection :: !Bool -- is direction visible to the user?
            , direction :: !PinDirection
            , activeLow :: !Bool
            , value :: !PinValue -- This is the line level
            , edge :: Maybe PinReadTrigger
            } deriving (Show, Eq)

-- | Default initial state of mock pins.
defaultState :: MockState
defaultState = MockState { exported = False
                         , hasUserDirection = True
                         , direction = Out
                         , activeLow = False
                         , value = Low
                         , edge = Just Disabled
                         }

-- | Maps pins to their state
type MockWorld = Map Pin MockState

-- | Construct a 'MockWorld' where each 'Pin' in the provided list is
-- available, unexported, and set to the 'defaultState'.
mockWorld :: [Pin] -> MockWorld
mockWorld pins = Map.fromList $ zip pins (repeat defaultState)

-- | A monad transformer which adds mock sysfs computations to an
-- inner monad 'm'.
newtype SysfsMockT m a =
  SysfsMockT { unSysfsMockT :: StateT MockWorld m a }
  deriving (Alternative,Applicative,Functor,Monad,MonadState MockWorld,MonadReader r,MonadWriter w,MonadFix,MonadPlus,MonadIO,MonadCont,MonadTrans)

-- | Run a mock sysfs computation with the given 'MockWorld', and
-- return a tuple containing the final value and final 'MockWorld'
-- state.
runSysfsMockT :: (MonadIO m) => SysfsMockT m a -> MockWorld -> m (a, MockWorld)
runSysfsMockT action world = runStateT (unSysfsMockT action) world

-- | A convenient specialization of 'SysfsT' which runs GPIO
-- computations in the mock sysfs environment, and returns results as
-- 'Either' 'String' 'a'.
type SysfsMock a = SysfsT (ExceptT String (SysfsMockT IO)) (ExceptT String (SysfsMockT IO)) a

-- | Run a 'SysfsT' computation in the mock syfs environment with the
-- given 'MockWorld', and return a tuple containing the final value
-- and final 'MockWorld' state. If an error occurs in the computation,
-- in the 'SysfsT' interpreter, or in the mock sysfs environment, it
-- will be thrown as an 'Control.Exception.Base.IOException'. In other
-- words, all errors will be expressed as
-- 'Control.Exception.Base.IOException's, just as a plain 'IO'
-- computation would do.
runSysfsMock :: SysfsMock a -> MockWorld -> IO (a, MockWorld)
runSysfsMock action world =
  runSysfsMock' action world >>= \case
    (Left e, _) -> fail e
    (Right a, w) -> return (a, w)

-- | Run a 'SysfsT' computation in the mock sysfs environment with the
-- given 'MockWorld', and return the result as 'Right' 'a', in
-- addition to the final 'MockWorld' state. If an error occurs in the
-- computation or in the interpreter, it is returned as 'Left'
-- 'String', in addition to the 'MockWorld' state at the time the
-- exception occurred. However, the function does not catch any
-- 'Control.Exception.Base.IOException's that occur as a side effect
-- of the computation; those will propagate upwards.
runSysfsMock' :: SysfsMock a -> MockWorld -> IO (Either String a, MockWorld)
runSysfsMock' action world = runSysfsMockT (runExceptT $ runSysfsT action) world

-- | Run a 'SysfsT' computation in the mock sysfs environment with the
-- given 'MockWorld', and return the result as a 'Right' value
-- containing a tuple of 'a' and the final 'MockWorld' state. If an
-- error occurs in the computation, in the 'SysfsT' interpreter, or
-- elsewhere while the computation is running (including
-- 'Control.Exception.Base.IOException's that occur as a side effect
-- of the computation) it is handled by this function and is returned
-- as an error result via 'Left' 'String'. (Unfortuntely, the
-- 'MockWorld' state at the time of the exception cannot be captured.)
-- Therefore, this function always returns a result (assuming the
-- computation terminates) and never throws an exception.
runSysfsMockSafe :: SysfsMock a -> MockWorld -> IO (Either String (a, MockWorld))
runSysfsMockSafe action world =
  runExceptT $ scriptIO (runSysfsMock action world)

instance (MonadIO m) => MonadSysfs (SysfsMockT m) where
  sysfsIsPresent = sysfsIsPresentMock
  pinIsExported = pinIsExportedMock
  pinHasDirection = pinHasDirectionMock
  pinHasEdge = pinHasEdgeMock
  exportPin = exportPinMock
  unexportPin = unexportPinMock
  readPinDirection = readPinDirectionMock
  writePinDirection = writePinDirectionMock
  writePinDirectionWithValue = writePinDirectionWithValueMock
  readPinValue = readPinValueMock
  threadWaitReadPinValue = undefined -- XXX dhess: add later
  writePinValue = writePinValueMock
  readPinEdge = readPinEdgeMock
  writePinEdge = writePinEdgeMock
  readPinActiveLow = readPinActiveLowMock
  writePinActiveLow = writePinActiveLowMock
  availablePins = availablePinsMock

sysfsIsPresentMock :: (Monad m) => SysfsMockT m Bool
sysfsIsPresentMock = return True

pinIsExportedMock :: (Monad m) => Pin -> SysfsMockT m Bool
pinIsExportedMock p =
  do s <- pinState p
     return $ maybe False exported s

-- The way this is implemented in 'MonadSysfs' is, if the pin is not
-- exported, or if the pin does not have a user-visible direction
-- attribute, it's 'False', otherwise 'True'.
pinHasDirectionMock :: (Monad m) => Pin -> SysfsMockT m Bool
pinHasDirectionMock p = userVisibleDirection p >>= return . isJust

-- The way this is implemented in 'MonadSysfs' is, if the pin is not
-- exported, or if the pin does not have an edge attribute, it's
-- 'False', otherwise 'True'.
pinHasEdgeMock :: (Monad m) => Pin -> SysfsMockT m Bool
pinHasEdgeMock p =
  guardedPinState p edge (isJust . edge) >>= return . isJust

exportPinMock :: (MonadIO m) => Pin -> SysfsMockT m ()
exportPinMock p =
  pinState p >>= \case
    Nothing -> ioErr exportErrorInvalidArgument
    Just s ->
      if (exported s)
         then ioErr exportErrorResourceBusy
         else updatePinState p (s { exported = True })

unexportPinMock :: (MonadIO m) => Pin -> SysfsMockT m ()
unexportPinMock p =
  do s <- checkedPinState p id unexportErrorInvalidArgument
     updatePinState p $ s { exported = False }

readPinDirectionMock :: (MonadIO m) => Pin -> SysfsMockT m String
readPinDirectionMock p =
  userVisibleDirection p >>= \case
    Nothing -> ioErr (readPinDirectionErrorNoSuchThing p)
    Just In -> return "in\n"
    Just Out -> return "out\n"

writePinDirectionMock :: (MonadIO m) => Pin -> PinDirection -> SysfsMockT m ()
writePinDirectionMock p dir =
  guardedPinState p id hasUserDirection >>= \case
    Nothing -> ioErr (writePinDirectionErrorNoSuchThing p)
    Just s -> updatePinState p (s { direction = dir })

writePinDirectionWithValueMock :: (MonadIO m) => Pin -> PinValue -> SysfsMockT m ()
writePinDirectionWithValueMock p v =
  guardedPinState p id hasUserDirection >>= \case
    Nothing -> ioErr (writePinDirectionErrorNoSuchThing p)
    Just s -> updatePinState p (s { direction = Out, value = logicalLevel (activeLow s) v})

readPinValueMock :: (MonadIO m) => Pin -> SysfsMockT m String
readPinValueMock p =
  do s <- checkedPinState p id (readPinValueErrorNoSuchThing p)
     case (logicalLevel (activeLow s) (value s)) of
       Low -> return "0\n"
       High -> return "1\n"

writePinValueMock :: (MonadIO m) => Pin -> PinValue -> SysfsMockT m ()
writePinValueMock p v =
  do s <- checkedPinState p id (writePinValueErrorNoSuchThing p)
     case (direction s) of
       In -> ioErr (writePinValueErrorPermissionDenied p)
       Out -> updatePinState p (s { value = logicalLevel (activeLow s) v })

readPinEdgeMock :: (MonadIO m) => Pin -> SysfsMockT m String
readPinEdgeMock p =
  checkedPinState p edge (readPinEdgeErrorNoSuchThing p) >>= \case
    Nothing -> ioErr (readPinEdgeErrorNoSuchThing p)
    Just Disabled -> return "none\n"
    Just RisingEdge -> return "rising\n"
    Just FallingEdge -> return "falling\n"
    Just Level -> return "both\n"

writePinEdgeMock :: (MonadIO m) => Pin -> PinReadTrigger -> SysfsMockT m ()
writePinEdgeMock p t =
  do s <- checkedPinState p id (writePinEdgeErrorNoSuchThing p)
     case (edge s) of
       Nothing -> ioErr (writePinEdgeErrorNoSuchThing p)
       Just _ -> updatePinState p (s { edge = Just t })

readPinActiveLowMock :: (MonadIO m) => Pin -> SysfsMockT m String
readPinActiveLowMock p =
  checkedPinState p activeLow (readPinActiveLowErrorNoSuchThing p) >>= \case
    False -> return "0\n"
    True -> return "1\n"

writePinActiveLowMock :: (MonadIO m) => Pin -> Bool -> SysfsMockT m ()
writePinActiveLowMock p v =
  do s <- checkedPinState p id (writePinActiveLowErrorNoSuchThing p)
     updatePinState p (s { activeLow = v})

-- XXX TODO: this function doesn't have any way to emulate the failure
-- of the hunt for the "base" and "ngpio" files in GPIO chip
-- subdirectories. Under normal conditions, those files will always
-- exist, but....
availablePinsMock :: (MonadIO m) => SysfsMockT m [Pin]
availablePinsMock =
  do hasSysfs <- sysfsIsPresentMock
     unless hasSysfs $
       ioErr availablePinsErrorNoSuchThing
     pins <- gets Map.keys
     return $ sort pins


-- Convenience functions
--

pinState :: (Monad m) => Pin -> SysfsMockT m (Maybe MockState)
pinState p =
  do pins <- get
     return $ Map.lookup p pins

updatePinState :: (Monad m) => Pin -> MockState -> SysfsMockT m ()
updatePinState p s = modify' $ \world ->
  Map.insert p s world

ioErr :: (MonadIO m) => IOError -> SysfsMockT m a
ioErr e = liftIO $ ioError e

logicalLevel :: Bool -> PinValue -> PinValue
logicalLevel False Low = Low
logicalLevel False High = High
logicalLevel True Low = High
logicalLevel True High = Low

-- If the 'Pin' is not exported, throw the given 'IOError'. Otherwise,
-- apply 'f' to its 'MockState' and return the result.
checkedPinState :: (MonadIO m) => Pin -> (MockState -> a) -> IOError -> SysfsMockT m a
checkedPinState p f e =
  guardedPinState p f (const True) >>= \case
    Nothing -> ioErr e
    Just s -> return s

-- If the 'Pin' is neither exported nor satisfies the given predicate,
-- return Nothing. Otherwise, apply 'f' to its 'MockState' and return
-- the result as 'Just' 'a'.
guardedPinState :: (Monad m) => Pin -> (MockState -> a) -> (MockState -> Bool) -> SysfsMockT m (Maybe a)
guardedPinState p f predicate =
  do pins <- get
     return $ guardedPinState' p pins f predicate

-- Pure version of 'guardedPinState'.
guardedPinState' :: Pin -> MockWorld -> (MockState -> a) -> (MockState -> Bool) -> Maybe a
guardedPinState' p stateMap f predicate =
  do s <- Map.lookup p stateMap
     guard $ exported s
     guard $ predicate s
     return $ f s

-- If the pin is exported and its "direction" attribute exists, return
-- the pin's direction in a 'Just'. Otherwise, return 'Nothing'.
userVisibleDirection :: (Monad m) => Pin -> SysfsMockT m (Maybe PinDirection)
userVisibleDirection p = guardedPinState p direction hasUserDirection


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

-- Edge file doesn't exist.
readPinEdgeErrorNoSuchThing :: Pin -> IOError
readPinEdgeErrorNoSuchThing p = mkErrorNoSuchThing "openFile" (pinEdgeFileName p)

writePinEdgeErrorNoSuchThing :: Pin -> IOError
writePinEdgeErrorNoSuchThing = readPinEdgeErrorNoSuchThing

-- Value file doesn't exist.
readPinValueErrorNoSuchThing :: Pin -> IOError
readPinValueErrorNoSuchThing p = mkErrorNoSuchThing "openFile" (pinValueFileName p)

writePinValueErrorNoSuchThing :: Pin -> IOError
writePinValueErrorNoSuchThing = readPinValueErrorNoSuchThing

-- Value file can't be written.
writePinValueErrorPermissionDenied :: Pin -> IOError
writePinValueErrorPermissionDenied p = mkErrorPermissionDenied "hClose" (pinValueFileName p)

-- Active low file doesn't exist.
readPinActiveLowErrorNoSuchThing :: Pin -> IOError
readPinActiveLowErrorNoSuchThing p = mkErrorNoSuchThing "openFile" (pinActiveLowFileName p)

writePinActiveLowErrorNoSuchThing :: Pin -> IOError
writePinActiveLowErrorNoSuchThing = readPinActiveLowErrorNoSuchThing

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
