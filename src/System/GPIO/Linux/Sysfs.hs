-- | GPIO in Linux via the 'sysfs' filesystem.

module System.GPIO.Linux.Sysfs
       ( -- * The Linux 'sysfs' GPIO interpreter
         -- | These types and functions provide the interpreter for
         -- running 'System.GPIO.Free.GpioF' eDSL programs on a Linux
         -- host with the 'sysfs' GPIO filesystem.
         --
         -- This interpreter has a pluggable 'sysfs' GPIO back-end,
         -- implemented as the @mtl@-style
         -- 'System.GPIO.Linux.Sysfs.Monad.MonadSysfs' type class.
         -- Therefore, you must combine this interpreter with a
         -- 'System.GPIO.Linux.Sysfs.Monad.MonadSysfs' instance.
         --
         -- Typically, you'll want to use the 'SysfsIO' monad (or its
         -- corresponding 'SysfsIOT' monad transformer) as the 'sysfs'
         -- GPIO back-end, which is designed to run on an actual Linux
         -- host and perform real GPIO operations. However, for
         -- testing purposes, this package also provides the
         -- 'System.GPIO.Linux.Sysfs.Mock.SysfsMock' monad, a crude
         -- but portable simulation of the 'sysfs' GPIO filesystem.
         -- (The 'System.GPIO.Linux.Sysfs.Mock.SysfsMock' monad is not
         -- exported from this module, but you can obtain it by
         -- importing the "System.GPIO.Linux.Sysfs.Mock" module.)
         SysfsF
       , SysfsT
       , runSysfsT
       , PinDescriptor(..)
         -- * The Linux 'sysfs' GPIO monad
         -- | Generally meant to be used with the 'SysfsT'
         -- interpreter.
       , SysfsIOT(..)
       , SysfsIO
       , runSysfsIO
         -- * Low-level 'sysfs' GPIO functions
         -- | If you don't want the overhead of an interpreter or a
         -- monad transformer stack, and are willing to give up
         -- portability, you can use these functions to write directly
         -- to the Linux 'sysfs' GPIO filesystem in 'System.IO.IO'.
       , sysfsIsPresent
       , availablePins
       , pinIsExported
       , exportPin
       , unexportPin
       , pinHasDirection
       , readPinDirection
       , writePinDirection
       , writePinDirectionWithValue
       , readPinValue
       , threadWaitReadPinValue
       , threadWaitReadPinValue'
       , writePinValue
       , pinHasEdge
       , readPinEdge
       , writePinEdge
       , readPinActiveLow
       , writePinActiveLow
         -- * 'sysfs'-specific types
       , SysfsEdge(..)
       , toPinReadTrigger
       , toSysfsEdge
         -- * Exceptions
       , SysfsException(..)
       ) where

import System.GPIO.Linux.Sysfs.Free
import System.GPIO.Linux.Sysfs.IO
import System.GPIO.Linux.Sysfs.Types
