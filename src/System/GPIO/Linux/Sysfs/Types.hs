{-|
Module      : System.GPIO.Linux.Sysfs.Types
Description : Types for Linux @sysfs@ GPIO
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
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
       , toPinReadTrigger
       , toSysfsEdge
         -- * Exceptions
       , SysfsException(..)
       ) where

import Control.Monad.Catch (Exception(..))
import Data.Data
import GHC.Generics
import System.GPIO.Types
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, genericShrink)

-- | Linux GPIO pins that can be configured to generate inputs have an
-- @edge@ attribute in the @sysfs@ GPIO filesystem. This type
-- represents the values that the @edge@ attribute can take.
--
-- This type is isomorphic to the 'PinReadTrigger' type. See
-- 'toPinReadTrigger' and 'toSysfsEdge'.
data SysfsEdge
  = None
  | Rising
  | Falling
  | Both
  deriving (Bounded,Enum,Eq,Data,Ord,Read,Show,Generic,Typeable)

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
    -- cannot be modified
  | AlreadyExported Pin
    -- ^ The pin has already been exported
  | InvalidPin Pin
    -- ^ The specified pin does not exist
  | NotExported Pin
    -- ^ The pin has been un-exported or does not exist
  | NoDirectionAttribute Pin
    -- ^ The pin does not have a @direction@ attribute
  | NoEdgeAttribute Pin
    -- ^ The pin does not have an @edge@ attribute
  | UnexpectedDirection Pin String
    -- ^ An unexpected value was read from the pin's @direction@
    -- attribute
  | UnexpectedValue Pin String
    -- ^ An unexpected value was read from the pin's @value@
    -- attribute
  | UnexpectedEdge Pin String
    -- ^ An unexpected value was read from the pin's @edge@
    -- attribute
  | UnexpectedActiveLow Pin String
    -- ^ An unexpected value was read from the pin's @active_low@
    -- attribute
  | UnexpectedContents FilePath String
    -- ^ An unexpected value was read from the pin's @active_low@
    -- attribute
  | InternalError String
    -- ^ An internal error has occurred in the interpreter, something
    -- which should "never happen" and should be reported to the
    -- package maintainer.
  deriving (Eq,Show,Typeable)

instance Exception SysfsException where
  toException = gpioExceptionToException
  fromException = gpioExceptionFromException
