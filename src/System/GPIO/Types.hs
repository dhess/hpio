-- | Basic GPIO types.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
         -- * PinValue <-> Bool conversions
       , valueToBool
       , boolToValue
       ) where

import Data.Bits
import Data.Data
import Data.Ix
import GHC.Generics
import Test.QuickCheck

-- | A GPIO pin, identified by pin number.
newtype Pin =
  Pin Int
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Ix,Show,Generic)

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
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Ix,Generic)

instance Arbitrary PinDirection where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | Pin value (high/low voltage).
data PinValue
  = Low
  | High
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Ix,Generic)

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

  countTrailingZeros Low  = 1
  countTrailingZeros High = 0

  countLeadingZeros = countTrailingZeros

instance Arbitrary PinValue where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | Pins can be configured so that reading the pin's value blocks
-- until an edge- or level-triggered event is detected.
data PinReadTrigger
  = RisingEdge
  | FallingEdge
  | Level
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Generic)

-- | Invert a 'PinDirection' value.
invertDirection :: PinDirection -> PinDirection
invertDirection In = Out
invertDirection Out = In

-- | Invert a 'PinValue'.
invertValue :: PinValue -> PinValue
invertValue = complement

valueToBool :: PinValue -> Bool
valueToBool Low  = False
valueToBool High = True

boolToValue :: Bool -> PinValue
boolToValue False = Low
boolToValue True  = High
