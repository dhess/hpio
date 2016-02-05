-- | Linux GPIO.
--
-- Because they're generally only used for testing, the
-- 'System.GPIO.Linux.SysfsMock.SysfsMockT' monad transformer and the
-- 'System.GPIO.Linux.MonadSysfs.MonadSysfs' type class are not
-- exported from here.

module System.GPIO.Linux
       ( -- * The Linux sysfs GPIO monad
         SysfsIOT(..)
       , SysfsIO
       , runSysfsIO
         -- * The Linux sysfs GPIO interpreter
       , SysfsF
       , SysfsT
       , runSysfsT
       , PinDescriptor(..)
         -- * 'sysfs'-specific types
       , SysfsEdge(..)
       , toPinReadTrigger
       , toSysfsEdge
         -- * Exceptions
       , SysfsException(..)
       ) where

import System.GPIO.Linux.Sysfs
import System.GPIO.Linux.SysfsIO
import System.GPIO.Linux.SysfsTypes
