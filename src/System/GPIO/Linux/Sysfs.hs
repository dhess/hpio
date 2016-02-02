-- | A 'GpioF' interpreter for Linux GPIO via sysfs.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs
       ( -- * MonadSysfs class
         MonadSysfs(..)
         -- * The Linux sysfs GPIO interpreter
       , SysfsF
       , SysfsT
       , runSysfsT
         -- * Exceptions
       , SysfsException(..)
         -- * Linux sysfs GPIO types
       , PinDescriptor(..)
       ) where

import Control.Monad (void)
import Control.Monad.Catch (Exception, MonadMask(..), MonadThrow(..), bracket, throwM)
import Control.Monad.Trans.Free (iterT)
import Data.Typeable (Typeable)
import System.GPIO.Free (GpioF(..), GpioT, openPin, closePin, samplePin, writePin, getPinDirection, setPinDirection)
import System.GPIO.Linux.MonadSysfs (MonadSysfs(..))
import System.GPIO.Types

-- | Exceptions that can be thrown by 'SysfsT' computations.
data SysfsException
  = SysfsNotPresent
  | UnexpectedDirection Pin
  | UnexpectedValue Pin
  | UnexpectedEdge Pin
  | UnexpectedActiveLow Pin
  deriving (Show,Typeable)

instance Exception SysfsException

-- | The sysfs interpreter's pin handle type. Currently it's just a
-- newtype wrapper around a 'Pin'. The constructor is exported for
-- convenience, but note that the implementation may change in future
-- versions of the package.
newtype PinDescriptor = PinDescriptor { pin :: Pin } deriving (Show, Eq, Ord)

-- | A monad transformer which adds Linux sysfs GPIO computations to
-- other monads.
type SysfsT m = GpioT PinDescriptor m

-- | The Linux sysfs GPIO DSL type.
type SysfsF m = GpioF PinDescriptor m

-- | Run a 'SysfsT' computation embedded in monad 'm' and return the
-- result. Errors that occur in the interpreter are thrown as
-- 'SysfsException' values. (Errors that could occur in the
-- interpreter are generally limited to reading unexpected results
-- from various sysfs GPIO control files.)
runSysfsT :: (MonadMask m, MonadThrow m, MonadSysfs m) => (SysfsT m) m a -> m a
runSysfsT = iterT run
  where
    run :: (MonadMask m, MonadThrow m, MonadSysfs m) => (SysfsF m) (m a) -> m a

    run (Pins next) =
      do hasSysfs <- sysfsIsPresent
         case hasSysfs of
           False -> next []
           True -> availablePins >>= next

    -- Export the pin. Note that it may already be exported, which we
    -- treat as success.
    run (OpenPin p next) =
      do hasSysfs <- sysfsIsPresent
         case hasSysfs of
           False -> throwM SysfsNotPresent
           True ->
             do exported <- pinIsExported p
                case exported of
                  True -> next $ PinDescriptor p
                  False ->
                    do exportPin p
                       next $ PinDescriptor p

    run (ClosePin d next) =
      do let p = pin d
         unexportPin p
         next

    run (GetPinDirection d next) =
      do let p = pin d
         settable <- pinHasDirection p
         case settable of
           False -> next Nothing
           True ->
             do dir <- readPinDirection p
                next $ Just dir

    run (SetPinDirection d dir next) =
      do let p = pin d
         writePinDirection p dir
         next

    run (TogglePinDirection d next) =
      do maybeDir <- runSysfsT $ getPinDirection d
         case maybeDir of
           Nothing -> next Nothing
           Just dir ->
             do let newDir = invertDirection dir
                void $ runSysfsT $ setPinDirection d newDir
                next $ Just newDir

    run (SamplePin d next) =
      do let p = pin d
         value <- readPinValue p
         next value

    run (ReadPin d next) =
      do let p = pin d
         value <- threadWaitReadPinValue p
         next value

    run (WritePin d v next) =
      do let p = pin d
         writePinValue p v
         next

    run (WritePin' d v next) =
      do let p = pin d
         writePinDirectionWithValue p v
         next

    run (TogglePinValue h next) =
      do val <- runSysfsT $ samplePin h
         let newVal = invertValue val
         void $ runSysfsT $ writePin h newVal
         next newVal

    run (GetPinReadTrigger d next) =
      do let p = pin d
         hasEdge <- pinHasEdge p
         case hasEdge of
           False -> next Nothing
           True ->
             do edge <- readPinEdge p
                next $ Just edge

    run (SetPinReadTrigger d trigger next) =
      do let p = pin d
         writePinEdge p trigger
         next

    -- N.B.: Linux's "active_low" is the opposite of the eDSL's
    -- "active level"!
    run (GetPinActiveLevel d next) =
      do let p = pin d
         activeLow <- readPinActiveLow p
         next $ boolToValue (not activeLow)

    -- N.B.: Linux's "active_low" is the opposite of the eDSL's
    -- "active level"!
    run (SetPinActiveLevel d v next) =
      do let p = pin d
         writePinActiveLow p $ valueToBool (invertValue v)
         next

    run (WithPin p block next) =
      bracket (runSysfsT $ openPin p)
              (\pd -> runSysfsT $ closePin pd)
              (\pd ->
                do a <- runSysfsT $ block pd
                   next a)
