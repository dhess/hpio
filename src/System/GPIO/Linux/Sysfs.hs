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
         -- | These types and functions provide the interpreter for
         -- running 'System.GPIO.Free.GpioF' eDSL programs on a Linux
         -- host with the @sysfs@ GPIO filesystem.
         --
         -- This interpreter has a pluggable @sysfs@ GPIO back-end,
         -- implemented as the @mtl@-style
         -- 'System.GPIO.Linux.Sysfs.Monad.MonadSysfs' type class.
         -- Therefore, you must combine this interpreter with a
         -- 'System.GPIO.Linux.Sysfs.Monad.MonadSysfs' instance in
         -- order to run it.
         --
         -- Typically, you'll want to use the 'SysfsIO' monad (or its
         -- corresponding 'SysfsIOT' monad transformer) as the @sysfs@
         -- GPIO back-end, which is designed to run on an actual Linux
         -- host and perform real GPIO operations.
         --
          -- (Currently, 'SysfsIO' is the only available
          -- 'System.GPIO.Linux.Sysfs.Monad.MonadSysfs' instance, but
          -- a "mock" instance for testing will be made available in
          -- the future.)
         SysfsF
       , SysfsT
       , runSysfsT
       , PinDescriptor(..)
         -- * The Linux @sysfs@ GPIO monad
         --
       , SysfsIOT(..)
       , SysfsIO
       , runSysfsIO
         -- * Low-level @sysfs@ GPIO functions
         --
         -- | If you don't want the overhead of the
         -- 'System.GPIO.Free.GpioF' eDSL interpreter, and are willing
         -- to give up portability, you can use these functions to
         -- write directly to the Linux @sysfs@ GPIO filesystem. You
         -- will still need to "run" them with a
         -- 'System.GPIO.Linux.Sysfs.Monad.MonadSysfs' instance,
         -- however.
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

import System.GPIO.Linux.Sysfs.Free
import System.GPIO.Linux.Sysfs.IO
import System.GPIO.Linux.Sysfs.Types
import System.GPIO.Linux.Sysfs.Native
