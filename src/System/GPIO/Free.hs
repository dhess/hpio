-- | An eDSL for GPIO computations.
--
-- To make it easier to use the eDSL in typical scenarios, the eDSL is
-- implemented as a functor for the 'FreeT' transformer.
--
-- As the acronym implies, GPIO means performing I/O; therefore
-- unexpected errors may occur during program excecution. The 'GpioF'
-- eDSL makes allowances for "expected" failures (e.g., attempting to
-- open a pin may fail), but how unxpected failures are handled is a
-- detail left to each particular interpreter. The interpreters
-- included in the 'gpio' package strive to cleanly separate errors
-- which occur in the program itself (semantic errors, e.g., writing
-- to a pin that is configured for input), errors which occur in the
-- interpreter implementation (e.g., reading an unexpected value from
-- a sytem-level GPIO interface), and errors which occur during I/O
-- (e.g., while still in use by the program, a pin is unexpectedly
-- unexported by the system or by another program.)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module System.GPIO.Free
       ( -- * The abstract GPIO eDSL
         GpioF(..)
       , pins
       , openPin
       , closePin
       , getPinDirection
       , setPinDirection
       , readPin
       , writePin
       , withPin
         -- * The GpioT monad transformer
       , GpioT
         -- * GPIO types
       , Pin(..)
       , PinDirection(..)
       , Value(..)
       ) where

import Control.Monad.Trans.Free (FreeT, MonadFree, liftF)
import Control.Monad.Free.TH (makeFreeCon)
import GHC.Generics

-- | A GPIO pin, identified by pin number.
data Pin = Pin Int deriving (Eq, Ord, Show, Generic)

-- | Pin direction (input/output).
data PinDirection = In | Out deriving (Eq, Show, Generic)

-- | Pin value (high/low voltage).
data Value = Low | High deriving (Eq, Enum, Ord, Show, Generic)

-- | Commands for the GPIO eDSL.
data GpioF e h m next where
  Pins :: ([Pin] -> next) -> GpioF e h m next
  OpenPin :: Pin -> (Either e h -> next) -> GpioF e h m next
  ClosePin :: h -> next -> GpioF e h m next
  GetPinDirection :: h -> (Maybe PinDirection -> next) -> GpioF e h m next
  SetPinDirection :: h -> PinDirection -> next -> GpioF e h m next
  ReadPin :: h -> (Value -> next) -> GpioF e h m next
  WritePin :: h -> Value -> next -> GpioF e h m next
  WithPin :: Pin -> (h -> GpioT e h m m a) -> (a -> next) -> GpioF e h m next

instance Functor (GpioF e h m) where
  fmap f (Pins g) = Pins (f . g)
  fmap f (OpenPin p g) = OpenPin p (f . g)
  fmap f (ClosePin h x) = ClosePin h (f x)
  fmap f (GetPinDirection h g) = GetPinDirection h (f . g)
  fmap f (SetPinDirection h dir x) = SetPinDirection h dir (f x)
  fmap f (ReadPin h g) = ReadPin h (f . g)
  fmap f (WritePin h v x) = WritePin h v (f x)
  fmap f (WithPin p block g) = WithPin p block (f . g)

-- | A transformer which adds GPIO programs to a monad stack. The 'e'
-- type parameter is an exception type for representing errors in the
-- program; 'h' is an abstract pin handle for operating on opened
-- pins; and 'm' is the wrapped monad.
--
-- Note that the 'm' parameter is required in order to implement the
-- 'WithPin' command, which runs programs within programs and is
-- therefore dependent on the wrapped monad type.
type GpioT e h m = FreeT (GpioF e h m)

-- | Get a list of available GPIO pins on the system.
--
-- This command makes a best-effort attempt to find the available
-- pins, but some systems may not make the complete list available at
-- runtime. Therefore, there may be more pins available than are
-- returned by this function.
makeFreeCon 'Pins

-- | Open a pin for reading and writing. If the pin can be opened,
-- the function returns a pin handle, which is used to operate on the
-- pin. If the pin cannot be opened (e.g., due to a permissions
-- failure), the function returns an error value.
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
-- 'getPinDirection' to make sure this particular pin's direction is
-- configurable.
makeFreeCon 'SetPinDirection

-- | Read the pin's 'Value'.
makeFreeCon 'ReadPin

-- | Set the pin's 'Value'. It is an error to call this function when
-- the pin is not configured for output.
makeFreeCon 'WritePin

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
