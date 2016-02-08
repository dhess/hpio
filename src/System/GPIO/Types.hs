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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module System.GPIO.Types
       ( -- * GPIO pins
         Pin(..)
       , PinDirection(..)
       , PinValue(..)
       , PinReadTrigger(..)
         -- * Convenience functions
       , pinNumber
       , invertDirection
       , invertValue
         -- * PinValue conversion to/from Bool
       , valueToBool
       , boolToValue
       ) where

import Data.Bits
import Data.Data
import Data.Ix
import Data.Typeable
import GHC.Generics
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, genericShrink)

-- | A GPIO pin, identified by pin number.
--
-- Note that GPIO pin numbering is platform- and runtime-dependent.
-- See the documentation for each implementation for an explanation of
-- how pin numbers are assigned to physical pins.
newtype Pin =
  Pin Int
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Ix,Show,Generic,Typeable)

instance Arbitrary Pin where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | Get the pin number as an 'Int'.
pinNumber :: Pin -> Int
pinNumber (Pin n) = n

-- | Pin direction (input/output).
data PinDirection
  = In
  | Out
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Ix,Generic,Typeable)

instance Arbitrary PinDirection where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | Pin value (high/low voltage).
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

-- | Pins can be configured so that reading the pin's value blocks
-- until an edge- or level-triggered event is detected. In this way, a
-- GPIO pin can be used as an edge- or level-triggered interrupt.
--
-- When the pin's read trigger is set to 'Disabled', reading the pin's
-- value will block indefinitely. (This is equivalent to
-- masking/disabling interrupts on the pin.)
data PinReadTrigger
  = Disabled
  | RisingEdge
  | FallingEdge
  | Level
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Generic,Typeable)

instance Arbitrary PinReadTrigger where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | Invert a 'PinDirection' value.
invertDirection :: PinDirection -> PinDirection
invertDirection In = Out
invertDirection Out = In

-- | Invert a 'PinValue'.
invertValue :: PinValue -> PinValue
invertValue = complement

-- | Convert a 'PinValue' to its logical boolean equivalent.
valueToBool :: PinValue -> Bool
valueToBool Low  = False
valueToBool High = True

-- | Convert a 'Bool' to its logical 'PinValue' equivalent.
boolToValue :: Bool -> PinValue
boolToValue False = Low
boolToValue True  = High

