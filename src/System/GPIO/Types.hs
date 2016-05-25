{-|
Module      : System.GPIO.Types
Description : Basic GPIO types
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Basic GPIO types.

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module System.GPIO.Types
       ( -- * GPIO pins
         Pin(..)
       , PinDirection(..)
       , PinActiveLevel(..)
       , PinValue(..)
       , PinInterruptMode(..)
         -- * Convenience functions
       , pinNumber
       , invertDirection
       , invertValue
         -- * PinValue conversion to/from Bool
       , valueToBool
       , boolToValue
         -- * GPIO exceptions
       , SomeGpioException(..)
       , gpioExceptionToException
       , gpioExceptionFromException
       ) where

import Control.Exception (Exception(..), SomeException)
import Data.Bits
import Data.Data
import Data.Ix
import GHC.Generics
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, genericShrink)

-- | A GPIO pin, identified by pin number.
--
-- Note that GPIO pin numbering is platform- and runtime-dependent.
-- See the documentation for your particular platform for an
-- explanation of how pin numbers are assigned to physical pins.
newtype Pin =
  Pin Int
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Ix,Show,Generic,Typeable)

instance Arbitrary Pin where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | Get the pin number as an 'Int'.
--
-- >>> pinNumber (Pin 5)
-- 5
pinNumber :: Pin -> Int
pinNumber (Pin n) = n

-- | A pin's direction (input/output).
data PinDirection
  = In
  | Out
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Ix,Generic,Typeable)

instance Arbitrary PinDirection where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | A pin's active level (active-high/active-low).
data PinActiveLevel
  = ActiveLow
  | ActiveHigh
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Ix,Generic,Typeable)

instance Arbitrary PinActiveLevel where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | A pin's signal level as a binary value.
data PinValue
  = Low
  | High
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Ix,Generic,Typeable)

instance Bits PinValue where
  High .&. High = High
  _    .&. _    = Low

  Low  .|. Low  = Low
  _    .|. _    = High

  Low  `xor` Low  = Low
  Low  `xor` High = High
  High `xor` Low  = High
  High `xor` High = Low

  complement Low = High
  complement High = Low

  shift x 0 = x
  shift _ _ = Low

  rotate x _ = x

  bit 0 = High
  bit _ = Low

  testBit x 0 = valueToBool x
  testBit _ _ = False

  bitSizeMaybe _ = Just 1

  bitSize _ = 1

  isSigned _ = False

  popCount Low  = 0
  popCount High = 1

instance FiniteBits PinValue where
  finiteBitSize _ = 1

#if MIN_VERSION_base(4,8,0)
  countTrailingZeros Low  = 1
  countTrailingZeros High = 0

  countLeadingZeros = countTrailingZeros
#endif

instance Arbitrary PinValue where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | A pin's interrupt mode.
--
-- Note that the pin's interrupt mode is defined in terms of the pin's
-- /logical/ signal value; i.e., when the pin is configured for
-- active-low logic, 'RisingEdge' refers to the physical signal's
-- trailing edge, and 'FallingEdge' refers to the physical signal's
-- rising edge.
data PinInterruptMode
  = Disabled
  -- ^ Interrupts are disabled
  | RisingEdge
  -- ^ Interrupt on the pin's (logical) rising edge
  | FallingEdge
  -- ^ Interrupt on the pin's (logical) falling edge
  | Level
  -- ^ Interrupt on any change to the pin's signal level
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Generic,Typeable)

instance Arbitrary PinInterruptMode where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | Invert a 'PinDirection' value.
--
-- >>> invertDirection In
-- Out
-- >>> invertDirection Out
-- In
invertDirection :: PinDirection -> PinDirection
invertDirection In = Out
invertDirection Out = In

-- | Invert a 'PinValue'.
--
-- >>> invertValue High
-- Low
-- >>> invertValue Low
-- High
invertValue :: PinValue -> PinValue
invertValue = complement

-- | Convert a 'PinValue' to its logical boolean equivalent.
--
-- >>> valueToBool High
-- True
-- >>> valueToBool Low
-- False
valueToBool :: PinValue -> Bool
valueToBool Low  = False
valueToBool High = True

-- | Convert a 'Bool' to its logical 'PinValue' equivalent.
--
-- >>> boolToValue True
-- High
-- >>> boolToValue False
-- Low
boolToValue :: Bool -> PinValue
boolToValue False = Low
boolToValue True  = High

-- | The top level of the GPIO exception hierarchy.
data SomeGpioException = forall e . Exception e => SomeGpioException e
    deriving Typeable

instance Show SomeGpioException where
    show (SomeGpioException e) = show e

instance Exception SomeGpioException

-- | Convert 'SomeGpioException' to 'SomeException'.
gpioExceptionToException :: Exception e => e -> SomeException
gpioExceptionToException = toException . SomeGpioException

-- | Ask whether an exception is 'SomeGpioException'.
gpioExceptionFromException :: Exception e => SomeException -> Maybe e
gpioExceptionFromException x = do
    SomeGpioException a <- fromException x
    cast a
