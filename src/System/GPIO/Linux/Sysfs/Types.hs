{-|
Module      : System.GPIO.Linux.Sysfs.Types
Description : Types for Linux @sysfs@ GPIO
Copyright   : (c) 2017, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <dhess-src@quixoftic.com>
Stability   : experimental
Portability : non-portable

Types used by the various Linux @sysfs@ GPIO implementations.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}

module System.GPIO.Linux.Sysfs.Types
       ( -- * @sysfs@-specific types
        SysfsEdge(..)
       , toPinInterruptMode
       , toSysfsEdge
         -- * Exceptions
       , SysfsException(..)
       ) where

import Protolude
import Control.Monad.Catch (Exception(..))
import Data.Data (Data)
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, genericShrink)

import System.GPIO.Types
       (Pin, PinInputMode, PinOutputMode, PinInterruptMode(..),
        gpioExceptionToException, gpioExceptionFromException)

-- | Linux GPIO pins that can be configured to generate inputs have an
-- @edge@ attribute in the @sysfs@ GPIO filesystem. This type
-- represents the values that the @edge@ attribute can take.
--
-- Note that in Linux @sysfs@ GPIO, the signal edge referred to by the
-- @edge@ attribute refers to the signal's /logical/ value; i.e., it
-- takes into account the value of the pin's @active_low@ attribute.
--
-- This type is isomorphic to the 'PinInterruptMode' type. See
-- 'toPinInterruptMode' and 'toSysfsEdge'.
data SysfsEdge
  = None
  -- ^ Interrupts disabled
  | Rising
  -- ^ Interrupt on the (logical) signal's rising edge
  | Falling
  -- ^ Interrupt on the (logical) signal's falling edge
  | Both
  -- ^ Interrupt on any change to the signal level
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Generic,Typeable)

instance Arbitrary SysfsEdge where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

-- | Convert a 'SysfsEdge' value to its equivalent 'PinInterruptMode'
-- value.
--
-- >>> toPinInterruptMode None
-- Disabled
-- >>> toPinInterruptMode Rising
-- RisingEdge
-- >>> toPinInterruptMode Falling
-- FallingEdge
-- >>> toPinInterruptMode Both
-- Level
toPinInterruptMode :: SysfsEdge -> PinInterruptMode
toPinInterruptMode None = Disabled
toPinInterruptMode Rising = RisingEdge
toPinInterruptMode Falling = FallingEdge
toPinInterruptMode Both = Level

-- | Convert a 'PinInterruptMode' value to its equivalent 'SysfsEdge'
-- value.
--
-- >>> toSysfsEdge Disabled
-- None
-- >>> toSysfsEdge RisingEdge
-- Rising
-- >>> toSysfsEdge FallingEdge
-- Falling
-- >>> toSysfsEdge Level
-- Both
toSysfsEdge :: PinInterruptMode -> SysfsEdge
toSysfsEdge Disabled = None
toSysfsEdge RisingEdge = Rising
toSysfsEdge FallingEdge = Falling
toSysfsEdge Level = Both

-- | Exceptions that can be thrown by @sysfs@ computations (in
-- addition to standard 'System.IO.Error.IOError' exceptions, of
-- course).
--
-- The @UnexpectedX@ values are truly exceptional and mean that, while
-- the @sysfs@ attribute for the given pin exists, the contents of the
-- attribute do not match any expected value for that attribute, which
-- probably means that the package is incompatible with the @sysfs@
-- filesystem due to a kernel-level change.
data SysfsException
  = SysfsNotPresent
    -- ^ The @sysfs@ filesystem does not exist
  | SysfsError
    -- ^ Something in the @sysfs@ filesystem does not behave as
    -- expected (could indicate a change in @sysfs@ behavior that the
    -- package does not expect)
  | SysfsPermissionDenied
    -- ^ The @sysfs@ operation is not permitted due to insufficient
    -- permissions
  | PermissionDenied Pin
    -- ^ The operation on the specified pin is not permitted, either
    -- due to insufficient permissions, or because the pin's attribute
    -- cannot be modified (e.g., trying to write to a pin that's
    -- configured for input)
  | InvalidOperation Pin
    -- ^ The operation is invalid for the specified pin, or in the
    -- specified pin's current configuration
  | AlreadyExported Pin
    -- ^ The pin has already been exported
  | InvalidPin Pin
    -- ^ The specified pin does not exist
  | NotExported Pin
    -- ^ The pin has been un-exported or does not exist
  | UnsupportedInputMode PinInputMode Pin
    -- ^ The pin does not support the specified input mode
  | UnsupportedOutputMode PinOutputMode Pin
    -- ^ The pin does not support the specified output mode
  | NoDirectionAttribute Pin
    -- ^ The pin does not have a @direction@ attribute
  | NoEdgeAttribute Pin
    -- ^ The pin does not have an @edge@ attribute
  | UnexpectedDirection Pin Text
    -- ^ An unexpected value was read from the pin's @direction@
    -- attribute
  | UnexpectedValue Pin Text
    -- ^ An unexpected value was read from the pin's @value@
    -- attribute
  | UnexpectedEdge Pin Text
    -- ^ An unexpected value was read from the pin's @edge@
    -- attribute
  | UnexpectedActiveLow Pin Text
    -- ^ An unexpected value was read from the pin's @active_low@
    -- attribute
  | UnexpectedContents FilePath Text
    -- ^ An unexpected value was read from the specified file
  | InternalError Text
    -- ^ An internal error has occurred in the interpreter, something
    -- which should "never happen" and should be reported to the
    -- package maintainer
  deriving (Eq,Show,Typeable)

instance Exception SysfsException where
  toException = gpioExceptionToException
  fromException = gpioExceptionFromException
