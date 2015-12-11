-- | An embedded DSL ("eDSL") for writing platform-independent GPIO
-- programs in Haskell.
--
-- Currently only the Linux "sysfs" GPIO system is supported, but
-- other platforms will be supported in the future.
--
-- Additionally, the package provides a "mock" GPIO environment for
-- testing programs on platforms that don't provide real GPIO
-- capabilities. That module is not exported from here, but you can
-- access it by directly importing "System.GPIO.Mock".

module System.GPIO
       ( -- * The abstract GPIO eDSL
         module System.GPIO.Free
         -- * The Linux sysfs GPIO interpreter
       , module System.GPIO.Linux.Sysfs
       ) where

import System.GPIO.Free
import System.GPIO.Linux.Sysfs (SysfsF, SysfsT, Sysfs, runSysfs, runSysfs', runSysfsSafe)
