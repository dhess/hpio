{-|
Module      : System.GPIO.Linux
Description : Linux GPIO
Copyright   : (c) 2018, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Linux GPIO.

Currently, this module is rather redundant, as it only re-exports the
top-level Linux @sysfs@ GPIO module. That's because @sysfs@ GPIO is
the only built-in GPIO implementation that the Linux kernel currently
supports. However, if future Linux kernels provide a new GPIO system,
that implementation would presumably also be exported from here.

-}

{-# LANGUAGE Safe #-}

module System.GPIO.Linux
       ( module System.GPIO.Linux.Sysfs
       ) where

import System.GPIO.Linux.Sysfs
