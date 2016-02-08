{-|
Module      : System.GPIO.Linux.Sysfs.Types
Description : Types for Linux 'sysfs' GPIO
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Types used by the various Linux 'sysfs' GPIO implementations.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}

module System.GPIO.Linux.Sysfs.Types
       ( -- * 'sysfs'-specific types
        SysfsEdge(..)
       , toPinReadTrigger
       , toSysfsEdge
         -- * Exceptions
       , SysfsException(..)
       ) where

import Control.Monad.Catch (Exception)
import Data.Data
import GHC.Generics
import System.GPIO.Types
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, genericShrink)

-- | Linux GPIO pins that can be configured to generate inputs have an
-- @edge@ attribute in the 'sysfs' GPIO filesystem. This type
-- represents the values that the @edge@ attribute can take.
--
-- This type is isomorphic to the 'PinReadTrigger' type in the
-- 'System.GPIO.Free.GpioF' eDSL.
data SysfsEdge
  = None
  | Rising
  | Falling
  | Both
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Generic)

instance Arbitrary SysfsEdge where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | Convert a 'SysfsEdge' value to its equivalent 'PinReadTrigger'
-- value.
toPinReadTrigger :: SysfsEdge -> PinReadTrigger
toPinReadTrigger None = Disabled
toPinReadTrigger Rising = RisingEdge
toPinReadTrigger Falling = FallingEdge
toPinReadTrigger Both = Level

-- | Convert a 'PinReadTrigger' value to its equivalent 'SysfsEdge'
-- value.
toSysfsEdge :: PinReadTrigger -> SysfsEdge
toSysfsEdge Disabled = None
toSysfsEdge RisingEdge = Rising
toSysfsEdge FallingEdge = Falling
toSysfsEdge Level = Both

-- | Exceptions that can be thrown by 'sysfs' computations (in
-- addition to standard 'System.IO.Error.IOError' exceptions, of
-- course).
--
-- The @UnexpectedX@ values are truly exceptional and mean that, while
-- the 'sysfs' attribute for the given pin exists, the contents of the
-- attribute do not match any expected value for that attribute. (This
-- would probably be indicative of a fundamental kernel-level GPIO
-- change or enhancement, and the need for an updated 'SysfsT'
-- interpreter to handle the new value(s).)
data SysfsException
  = SysfsNotPresent
  | UnexpectedDirection Pin String
  | UnexpectedValue Pin String
  | UnexpectedEdge Pin String
  | UnexpectedActiveLow Pin String
  | UnexpectedContents FilePath String
  deriving (Show,Typeable)

instance Exception SysfsException
