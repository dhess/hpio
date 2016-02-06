-- | Linux GPIO.
--
-- Currently, this module is rather redundant, as it only re-exports
-- the top-level Linux 'sysfs' GPIO module. That's because 'sysfs'
-- GPIO is the only built-in GPIO implementation that the Linux kernel
-- currently supports. However, if future Linux kernels provide a new
-- GPIO system, that implementation would presumably also be exported
-- from here.

module System.GPIO.Linux
       ( -- * Linux 'sysfs' GPIO
         module System.GPIO.Linux.Sysfs
       ) where

import System.GPIO.Linux.Sysfs
