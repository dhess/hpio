{-|
Module      : System.GPIO.Linux.Sysfs
Description : GPIO in Linux via the @sysfs@ filesystem
Copyright   : (c) 2017, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <dhess-src@quixoftic.com>
Stability   : experimental
Portability : non-portable

GPIO in Linux via the @sysfs@ filesystem.

See the <https://www.kernel.org/doc/Documentation/gpio/sysfs.txt Linux kernel documentation>
for the definitive description of the Linux @sysfs@-based GPIO API and
the terminology used in this module.

== Pin numbering

The @sysfs@ GPIO implementation in this module uses the same pin
numbering scheme as the @sysfs@ GPIO filesystem. For example,
'System.GPIO.Types.Pin' @13@ corresponds to @gpio13@ in the @sysfs@
filesystem. Note that the @sysfs@ pin numbering scheme is almost
always different than the pin numbering scheme given by the
platform/hardware documentation. Consult your platform documentation
for the mapping of pin numbers between the two namespaces.

-}

{-# LANGUAGE Safe #-}

module System.GPIO.Linux.Sysfs
       ( -- * The Linux @sysfs@ GPIO interpreter
         --
         -- | The 'SysfsGpioT' monad transformer provides an instance
         -- of the 'System.GPIO.Monad.MonadGpio' monad type class for
         -- running GPIO computations on a Linux host via the @sysfs@
         -- GPIO filesystem.
         --
         -- The implementation abstracts back-end @sysfs@ filesystem
         -- operations via the
         -- 'System.GPIO.Linux.Sysfs.Monad.MonadSysfs' monad type
         -- class. Primarily, this abstraction exists in order to more
         -- easily test @sysfs@ GPIO programs on non-Linux systems, or
         -- on Linux systems which lack actual GPIO functionality. To
         -- run GPIO programs on real GPIO-capable Linux systems,
         -- you'll want to combine the 'SysfsGpioT' transformer with
         -- the 'SysfsIOT' monad transformer. For the straightforward
         -- case of running @sysfs@ GPIO operations directly in 'IO',
         -- use the provided 'runSysfsGpioIO' wrapper; for more
         -- complicated transformer stacks, compose the
         -- 'runSysfsGpioT' and 'runSysfsIOT' wrappers. (See the
         -- "System.GPIO.Tutorial" module for details.)
         --
         -- For testing purposes, you can use the
         -- 'System.GPIO.Linux.Sysfs.Mock.SysfsMock' monad (or its
         -- corresponding 'System.GPIO.Linux.Sysfs.Mock.SysfsMockT'
         -- monad transformer) as the @sysfs@ back-end, which allows
         -- you to run (mock) GPIO programs on any system. Note that
         -- the testing monads are not exported from this module; you
         -- must import the "System.GPIO.Linux.Sysfs.Mock" module
         -- directly.
         SysfsGpioT
       , runSysfsGpioT
       , SysfsGpioIO
       , runSysfsGpioIO
       , PinDescriptor(..)
         -- * The Linux @sysfs@ monad
         --
       , MonadSysfs(..)
       , SysfsIOT(..)
         -- * Low-level @sysfs@ GPIO actions
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
       , exportPinChecked
       , unexportPin
       , unexportPinChecked
       , pinHasDirection
       , readPinDirection
       , writePinDirection
       , writePinDirectionWithValue
       , readPinValue
       , pollPinValue
       , pollPinValueTimeout
       , writePinValue
       , pinHasEdge
       , readPinEdge
       , writePinEdge
       , readPinActiveLow
       , writePinActiveLow
         -- * @sysfs@-specific types
       , SysfsEdge(..)
       , toPinInterruptMode
       , toSysfsEdge
         -- * @sysfs@-specific Exceptions
       , SysfsException(..)
       ) where

import System.GPIO.Linux.Sysfs.Monad
import System.GPIO.Linux.Sysfs.IO
import System.GPIO.Linux.Sysfs.Types

-- | A specialization of 'SysfsGpioT' which runs GPIO computations in
-- 'IO' via @sysfs@.
type SysfsGpioIO = SysfsGpioT (SysfsIOT IO)

-- | Run GPIO computations in 'IO' via @sysfs@.
runSysfsGpioIO :: SysfsGpioIO a -> IO a
runSysfsGpioIO action = runSysfsIOT $ runSysfsGpioT action
