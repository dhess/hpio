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
       , PinInputMode(..)
       , PinOutputMode(..)
       , PinCapabilities(..)
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
import Data.Set (Set)
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

-- | Input pins may support a number of different physical
-- configurations.
--
-- Pins that are capable of input will at least support the
-- 'InputDefault' mode.
--
-- Note that in 'InputDefault' mode, under the covers the pin's actual
-- input mode is most likely one of the other, more specific modes. By
-- using 'InputDefault' mode, you are simply saying that you don't
-- care about the pin's physical configuration, just that the pin is
-- being used for input.
data PinInputMode
  = InputDefault
    -- ^ The pin's default input mode, i.e., the mode used when a more
    -- specific mode is not specified
  | InputFloating
    -- ^ A floating \/ high-impedance \/ tri-state mode which uses
    -- little power, but when disconnected, may cause the pin's value
    -- to be indeterminate
  | InputPullUp
    -- ^ The pin is connected to an internal pull-up resistor such
    -- that, when the pin is disconnected or connected to a floating /
    -- high-impedance node, its physical value will be 'High'
  | InputPullDown
    -- ^ The pin is connected to an internal pull-down resistor such
    -- that, when the pin is disconnected or connected to a floating /
    -- high-impedance node, its physical value will be 'Low'
  deriving (Bounded,Enum,Eq,Ord,Data,Read,Show,Generic,Typeable)

-- | Output pins may support a number of different physical
-- configurations.
--
-- Pins that are capable of output will at least support the
-- 'OutputDefault' mode.
--
-- Note that in 'OutputDefault' mode, under the covers the pin's
-- actual input mode is most likely one of the other, more specific
-- modes. By using 'OutputDefault' mode, you are simply saying that
-- you don't care about the pin's physical configuration, just that
-- the pin is being used for output.
data PinOutputMode
  = OutputDefault
    -- ^ The pin's default output mode, i.e., the mode used when a
    -- more specific mode is not specified
  | OutputPushPull
    -- ^ The output actively drives both the 'High' and 'Low' states
  | OutputOpenDrain
    -- ^ The output actively drives the 'Low' state, but 'High' is
    -- left floating (also known as /open collector/)
  | OutputOpenDrainPullUp
    -- ^ The output actively drives the 'Low' state, and is connected
    -- to an internal pull-up resistor in the 'High' state.
  | OutputOpenSource
    -- ^ The output actively drives the 'High' state, but 'Low' is
    -- left floating (also known as /open emitter/)
  | OutputOpenSourcePullDown
    -- ^ The output actively drives the 'High' state, and is connected
    -- to an internal pull-down resistor in the 'Low' state.
  deriving (Bounded,Enum,Eq,Ord,Data,Read,Show,Generic,Typeable)

-- | Catalog a pin's capabilities.
data PinCapabilities =
  PinCapabilities {_inputModes :: Set PinInputMode
                   -- ^ Which input modes does the pin support?
                  ,_outputModes :: Set PinOutputMode
                   -- ^ Which output modes does the pin support?
                  ,_interrupts :: Bool
                   -- ^ Does the pin support interrupts in input mode?
                  }
  deriving (Eq,Show,Generic,Typeable)

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
