-- | A 'GpioF' interpreter for Linux GPIO using the 'sysfs'
-- filesystem.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module System.GPIO.Linux.Sysfs.Free
       ( -- * The Linux 'sysfs' GPIO interpreter
         SysfsF
       , SysfsT
       , runSysfsT
         -- * Linux 'sysfs' GPIO types
       , PinDescriptor(..)
       ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask(..), MonadThrow(..), bracket, throwM)
import Control.Monad.Trans.Free (iterT)
import System.GPIO.Free (GpioF(..), GpioT, openPin, closePin, samplePin, writePin, getPinDirection, setPinDirection)
import System.GPIO.Linux.Sysfs.Monad (MonadSysfs(..))
import System.GPIO.Types
import System.GPIO.Linux.Sysfs.Types (SysfsException( SysfsNotPresent ), toPinReadTrigger, toSysfsEdge)

-- | The 'sysfs' interpreter's pin handle type. Currently it's just a
-- newtype wrapper around a 'Pin'. The constructor is exported for
-- convenience, but note that the implementation may change in future
-- versions of the package.
newtype PinDescriptor = PinDescriptor { _pin :: Pin } deriving (Show, Eq, Ord)

-- | A monad transformer which adds 'GpioF' eDSL programs to other
-- monads.
type SysfsT m = GpioT PinDescriptor m

-- | A 'GpioF' eDSL type for Linux 'sysfs'-based GPIO.
type SysfsF m = GpioF PinDescriptor m

-- | Run (interpret) a 'SysfsT' computation embedded in monad 'm' and
-- return the result.
--
-- This function interprets cross-platform 'GpioF' eDSL programs by
-- translating 'GpioF' commands to their native equivalents on the
-- Linux 'sysfs' GPIO filesystem. The actual 'sysfs' GPIO operations
-- are provided by the wrapped monad 'm', which must be an instance of
-- 'MonadSysfs'.
--
-- Errors that occur in the interpreter are thrown as 'SysfsException'
-- values. Errors that could occur in the interpreter are generally
-- limited to reading unexpected results from various 'sysfs' GPIO
-- control files.
runSysfsT :: (MonadMask m, MonadThrow m, MonadSysfs m) => (SysfsT m) m a -> m a
runSysfsT = iterT run
  where
    run :: (MonadMask m, MonadThrow m, MonadSysfs m) => (SysfsF m) (m a) -> m a

    run (Pins next) =
      sysfsIsPresent >>= \case
        False -> next []
        True -> availablePins >>= next

    -- Export the pin. Note that it may already be exported, which we
    -- treat as success.
    run (OpenPin p next) =
      sysfsIsPresent >>= \case
        False -> throwM SysfsNotPresent
        True ->
          do exportPin' p
             next $ PinDescriptor p

    run (ClosePin (PinDescriptor p) next) =
      do unexportPin p
         next

    run (GetPinDirection (PinDescriptor p) next) =
      pinHasDirection p >>= \case
        False -> next Nothing
        True ->
          do dir <- readPinDirection p
             next $ Just dir

    run (SetPinDirection (PinDescriptor p) dir next) =
      do writePinDirection p dir
         next

    run (TogglePinDirection h next) =
      (runSysfsT $ getPinDirection h) >>= \case
        Nothing -> next Nothing
        Just dir ->
          do let newDir = invertDirection dir
             void $ runSysfsT $ setPinDirection h newDir
             next $ Just newDir

    run (SamplePin (PinDescriptor p) next) =
      do value <- readPinValue p
         next value

    run (ReadPin (PinDescriptor p) next) =
      do value <- threadWaitReadPinValue p
         next value

    run (ReadPinTimeout (PinDescriptor p) timeout next) =
      do value <- threadWaitReadPinValue' p timeout
         next value

    run (WritePin (PinDescriptor p) v next) =
      do writePinValue p v
         next

    run (WritePin' (PinDescriptor p) v next) =
      do writePinDirectionWithValue p v
         next

    run (TogglePinValue h next) =
      do val <- runSysfsT $ samplePin h
         let newVal = invertValue val
         void $ runSysfsT $ writePin h newVal
         next newVal

    run (GetPinReadTrigger (PinDescriptor p) next) =
      pinHasEdge p >>= \case
        False -> next Nothing
        True ->
          do edge <- readPinEdge p
             next $ Just (toPinReadTrigger edge)

    run (SetPinReadTrigger (PinDescriptor p) trigger next) =
      do writePinEdge p $ toSysfsEdge trigger
         next

    -- N.B.: Linux's "active_low" is the opposite of the eDSL's
    -- "active level"!
    run (GetPinActiveLevel (PinDescriptor p) next) =
      do activeLow <- readPinActiveLow p
         next $ boolToValue (not activeLow)

    -- N.B.: Linux's "active_low" is the opposite of the eDSL's
    -- "active level"!
    run (SetPinActiveLevel (PinDescriptor p) v next) =
      do writePinActiveLow p $ valueToBool (invertValue v)
         next

    run (WithPin p block next) =
      bracket (runSysfsT $ openPin p)
              (\pd -> runSysfsT $ closePin pd)
              (\pd ->
                do a <- runSysfsT $ block pd
                   next a)
