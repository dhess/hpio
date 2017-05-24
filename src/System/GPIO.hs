{-|
Module      : System.GPIO
Description : Top-level re-exports for writing GPIO programs
Copyright   : (c) 2017, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <dhess-src@quixoftic.com>
Stability   : experimental
Portability : non-portable

Top-level re-exports for writing GPIO programs.

-}

{-# LANGUAGE Safe #-}

module System.GPIO
       ( -- * The MonadGpio class
         module System.GPIO.Monad
         -- * GPIO in Linux
       , module System.GPIO.Linux
       ) where

import System.GPIO.Monad
import System.GPIO.Linux
