-- | An eDSL for GPIO computations.
--
-- To make it easier to use the eDSL in typical scenarios, the eDSL is
-- implemented as a functor for the 'FreeT' transformer.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module System.GPIO.Free
       ( -- * The GpioT monad transformer
         GpioT
         -- * The abstract GPIO eDSL
       , GpioF(..)
       , pins
       , openPin
       , closePin
       , getPinDirection
       , setPinDirection
       , togglePinDirection
       , samplePin
       , readPin
       , writePin
       , writePin'
       , togglePinValue
       , getPinReadTrigger
       , setPinReadTrigger
       , getPinActiveLevel
       , setPinActiveLevel
       , withPin
       ) where

import System.GPIO.Types

import Control.Monad.Trans.Free (FreeT, MonadFree, liftF)
import Control.Monad.Free.TH (makeFreeCon)

-- | Commands for the GPIO eDSL.
data GpioF h m next where
  Pins :: ([Pin] -> next) -> GpioF h m next
  OpenPin :: Pin -> (h -> next) -> GpioF h m next
  ClosePin :: h -> next -> GpioF h m next
  GetPinDirection :: h -> (Maybe PinDirection -> next) -> GpioF h m next
  SetPinDirection :: h -> PinDirection -> next -> GpioF h m next
  TogglePinDirection :: h -> (Maybe PinDirection -> next) -> GpioF h m next
  SamplePin :: h -> (PinValue -> next) -> GpioF h m next
  ReadPin :: h -> (PinValue -> next) -> GpioF h m next
  WritePin :: h -> PinValue -> next -> GpioF h m next
  WritePin' :: h -> PinValue -> next -> GpioF h m next
  TogglePinValue :: h -> (PinValue -> next) -> GpioF h m next
  GetPinReadTrigger :: h -> (Maybe PinReadTrigger -> next) -> GpioF h m next
  SetPinReadTrigger :: h -> PinReadTrigger -> next -> GpioF h m next
  GetPinActiveLevel :: h -> (PinValue -> next) -> GpioF h m next
  SetPinActiveLevel :: h -> PinValue -> next -> GpioF h m next
  WithPin :: Pin -> (h -> GpioT h m m a) -> (a -> next) -> GpioF h m next

instance Functor (GpioF h m) where
  fmap f (Pins g) = Pins (f . g)
  fmap f (OpenPin p g) = OpenPin p (f . g)
  fmap f (ClosePin h x) = ClosePin h (f x)
  fmap f (GetPinDirection h g) = GetPinDirection h (f . g)
  fmap f (SetPinDirection h dir x) = SetPinDirection h dir (f x)
  fmap f (TogglePinDirection h g) = TogglePinDirection h (f . g)
  fmap f (SamplePin h g) = SamplePin h (f . g)
  fmap f (ReadPin h g) = ReadPin h (f . g)
  fmap f (WritePin h v x) = WritePin h v (f x)
  fmap f (WritePin' h v x) = WritePin' h v (f x)
  fmap f (TogglePinValue h g) = TogglePinValue h (f . g)
  fmap f (GetPinReadTrigger h g) = GetPinReadTrigger h (f . g)
  fmap f (SetPinReadTrigger h t x) = SetPinReadTrigger h t (f x)
  fmap f (GetPinActiveLevel h g) = GetPinActiveLevel h (f . g)
  fmap f (SetPinActiveLevel h v x) = SetPinActiveLevel h v (f x)
  fmap f (WithPin p block g) = WithPin p block (f . g)

-- | A transformer which adds GPIO programs to a monad stack. 'h' is
-- an abstract pin handle for operating on opened pins, and 'm' is the
-- wrapped monad.
--
-- Note that the 'm' parameter is required in order to implement the
-- 'WithPin' command, which runs programs within programs and is
-- therefore dependent on the wrapped monad type.
type GpioT h m = FreeT (GpioF h m)

-- | Get a list of available GPIO pins on the system.
--
-- This command makes a best-effort attempt to find the available
-- pins, but some systems may not make the complete list available at
-- runtime. Therefore, there may be more pins available than are
-- returned by this function.
makeFreeCon 'Pins

-- | Open a pin for use and return a handle to it.
makeFreeCon 'OpenPin

-- | Close a pin; i.e., indicate to the system that you no longer
-- intend to use the pin via the given handle.
makeFreeCon 'ClosePin

-- | Get the pin's 'PinDirection'.
--
-- Note that it is not uncommon for a pin's direction to be immutable,
-- i.e., to be hard-wired as 'In' or 'Out'. On some systems (e.g.,
-- "System.GPIO.Linux.Sysfs"), the direction of hard-wired pins is not
-- made available at run-time. In such cases, this function returns
-- 'Nothing'.
makeFreeCon 'GetPinDirection

-- | Set the pin's 'PinDirection'.
--
-- As some pins' direction cannot be changed, you should first call
-- 'getPinDirection' on the pin handle to make sure this particular
-- pin's direction is configurable. It is an error to call this
-- function if the pin's direction cannot be changed.
makeFreeCon 'SetPinDirection

-- | Toggle the pin's 'PinDirection'.
--
-- If the pin's direction cannot be changed, this function returns
-- 'Nothing'. Otherwise, it returns the new direction.
makeFreeCon 'TogglePinDirection

-- | Sample the pin's /logical/ 'PinValue', where "sample" means "read
-- the value without blocking." Note that the pin's /physical/ line
-- level is the inverse of the returned value when the pin's active
-- level is 'Low'. (See 'getPinActiveLevel'.)
makeFreeCon 'SamplePin

-- | Read the pin's /logical/ 'PinValue', potentially blocking the
-- current thread until an event corresponding to the pin's
-- 'PinReadTrigger' occurs.
--
-- If the pin does not support blocking reads, then this function
-- behaves like 'samplePin' and returns the pin's logical value
-- without blocking.
--
-- Note: due to its interaction with the threading system, this
-- function may behave differently across different implementations of
-- Haskell. It has only been tested with GHC.
makeFreeCon 'ReadPin

-- | Set the pin's /logical/ 'PinValue'. It is an error to call this
-- function when the pin is not configured for output.
makeFreeCon 'WritePin

-- | Configure the pin for output and simultaneously set its
-- 'PinValue'. As long as the pin can be configured for output, you
-- can call this function regardless of the pin's current
-- 'PinDirection'. If the pin cannot be configured for output, it is
-- an error to call this function. (See 'getPinDirection' to determine
-- safely whether the pin can be configured for output.)
--
-- On some platforms (e.g., Linux 'sysfs' GPIO), this operation is
-- atomic, permitting glitch-free operation when configuring an output
-- pin's initial value. If the platform can't guarantee atomic
-- operation, this command is performed as two separate steps (first
-- setting the direction to 'Out', and then setting the 'PinValue').
makeFreeCon 'WritePin'

-- | Toggle the pin's 'PinValue'. It is an error to call this function
-- when the pin is not configured for output.
makeFreeCon 'TogglePinValue

-- | Get the pin's 'PinReadTrigger' mode.
--
-- Some pins (or GPIO platforms) may not support edge- or
-- level-triggered blocking reads. In such cases, this function
-- returns 'Nothing'.
makeFreeCon 'GetPinReadTrigger

-- | Set the pin's 'PinReadTrigger' mode.
--
-- Some pins (or entire platforms) may not support edge- or
-- level-triggered blocking reads. In such cases, it is an error to
-- call this function. To determine whether a pin supports blocking
-- reads, call 'getReadPinTrigger' on the pin.
makeFreeCon 'SetPinReadTrigger

-- | Get the pin's "active" level: 'Low' means the pin is configured
-- to be active low, and 'High' means the pin is configured to be
-- active high.
makeFreeCon 'GetPinActiveLevel

-- | Set the pin's "active" level, 'Low' or 'High'.
makeFreeCon 'SetPinActiveLevel

-- | Exception-safe pin management.
--
-- 'withPin' opens a pin using 'openPin' and passes the handle to the
-- given GPIO computation. Upon completion of the computation or an
-- exception occuring within the computation, 'withPin' closes the
-- handle using 'closePin' and then propagates the result, either by
-- returning the value of the computation or by re-raising the
-- exception. If closing the handle raises an exception, this
-- exception will be raised by 'withPin'.
--
-- Note: because the eDSL is abstract, it can make no guarantees about
-- the implementation of 'withPin' as that is left to each particular
-- interpreter. However, interpreters should strive to ensure that the
-- pin is closed properly no matter what kind of exception occurs,
-- regardless of how that exception is then expressed to the caller by
-- the implementation.
makeFreeCon 'WithPin
