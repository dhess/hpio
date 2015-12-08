-- Compile from top-level with "ghc -isrc"

module Main where

import System.GPIO.Free
import System.GPIO.Linux.Sysfs (Sysfs, runSysfs)

getPins :: Sysfs [Pin]
getPins = pins

main :: IO ()
main =
  do availablePins <- runSysfs getPins
     print availablePins
