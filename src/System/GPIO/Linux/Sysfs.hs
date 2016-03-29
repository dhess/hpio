{-|
Module      : System.GPIO.Linux.Sysfs
Description : GPIO in Linux via the @sysfs@ filesystem
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

GPIO in Linux via the @sysfs@ filesystem.

See the <https://www.kernel.org/doc/Documentation/gpio/sysfs.txt Linux kernel documentation>
for the definitive description of the Linux @sysfs@-based GPIO API and
the terminology used in this module.

== Pin numbering

The @sysfs@ GPIO implementation in this module uses the same pin
numbering scheme as the @sysfs@ GPIO filesystem. For example, 'Pin' 13
corresponds to @gpio13@ in the @sysfs@ filesystem. Note that the
@sysfs@ pin numbering scheme is almost always different than the pin
numbering scheme given by the platform/hardware documentation. Consult
your platform documentation for the mapping of pin numbers between the
two namespaces.

-}

{-# LANGUAGE Safe #-}

module System.GPIO.Linux.Sysfs
       ( -- * The Linux @sysfs@ GPIO interpreter
         --
         -- | These types and functions provide an instance of the
         -- 'System.GPIO.Monad.MonadGpio' monad type class for running
         -- GPIO computations on a Linux host via the @sysfs@ GPIO
         -- filesystem.
         --
         -- The implementation abstracts back-end @sysfs@ filesystem
         -- operations via the
         -- 'System.GPIO.Linux.Sysfs.Monad.MonadSysfs' monad type
         -- class. Primarily, this abstraction exists in order to more
         -- easily test @sysfs@ GPIO programs on non-Linux systems, or
         -- on Linux systems which lack actual GPIO functionality.
         --
         -- For real GPIO programs on real GPIO systems, you'll want
         -- to use the 'SysfsIO' monad (or its corresponding
         -- 'SysfsIOT' monad transformer) as the @sysfs@ back-end,
         -- which is designed to run on an actual Linux host and
         -- perform real GPIO operations.
         --
         -- For testing purposes, you can use the
         -- 'System.GPIO.Linux.Sysfs.Mock.SysfsMock' monad (or its
         -- corresponding 'System.GPIO.Linux.Sysfs.Mock.SysfsMockT'
         -- monad transformer) as the @sysfs@ back-end, which allows
         -- you to run (mock) GPIO programs on any system. (Note that
         -- these monads are not exported from here.)
         SysfsGpioT
       , runSysfsGpioT
       , PinDescriptor(..)
       , SysfsGpioIO
       , runSysfsGpioIO
         -- * The Linux @sysfs@ monad
         --
       , MonadSysfs(..)
       , SysfsIOT(..)
         -- * Low-level @sysfs@ GPIO functions
         --
         -- | A slightly more low-level API is also available if you
         -- want to write directly to the Linux @sysfs@ GPIO
         -- filesystem, or do something that the
         -- 'System.GPIO.Monad.MonadGpio' portable GPIO interface
         -- doesn't allow you to express.
       , sysfsIsPresent
       , availablePins
       , pinIsExported
       , exportPin
       , exportPin'
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
         -- * @sysfs@-specific types
       , SysfsEdge(..)
       , toPinReadTrigger
       , toSysfsEdge
         -- * Exceptions
       , SysfsException(..)
       ) where

import System.GPIO.Linux.Sysfs.Monad
import System.GPIO.Linux.Sysfs.IO
import System.GPIO.Linux.Sysfs.Types

-- | A specialization of 'SysfsGpioT' which runs (real) GPIO
-- computations in 'IO' via @sysfs@.
type SysfsGpioIO = SysfsGpioT (SysfsIOT IO)

-- | Run (real) GPIO computations in 'IO' via @sysfs@.
runSysfsGpioIO :: SysfsGpioIO a -> IO a
runSysfsGpioIO action = runSysfsIOT $ runSysfsGpioT action
