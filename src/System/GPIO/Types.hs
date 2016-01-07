-- | Basic GPIO types.

{-# LANGUAGE DeriveGeneric #-}

module System.GPIO.Types
       ( -- * GPIO pins
         Pin(..)
       , PinDirection(..)
       , PinValue(..)
         -- * Convenience functions
       , invertDirection
       , invertValue
         -- * PinValue <-> Bool conversions
       , valueToBool
       , boolToValue
       ) where

import GHC.Generics

-- | A GPIO pin, identified by pin number.
data Pin = Pin Int deriving (Eq, Ord, Show, Generic)

-- | Pin direction (input/output).
data PinDirection = In | Out deriving (Eq, Show, Generic)

-- | Pin value (high/low voltage).
data PinValue = Low | High deriving (Eq, Enum, Ord, Show, Generic)

-- | Invert a 'PinDirection' value.
invertDirection :: PinDirection -> PinDirection
invertDirection In = Out
invertDirection Out = In

-- | Invert a 'PinValue'.
invertValue :: PinValue -> PinValue
invertValue High = Low
invertValue Low = High

valueToBool :: PinValue -> Bool
valueToBool Low  = False
valueToBool High = True

boolToValue :: Bool -> PinValue
boolToValue False = Low
boolToValue True  = High
