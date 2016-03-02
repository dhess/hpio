{-|
Module      : System.GPIO.Linux.Sysfs.Util
Description : Useful low-level Linux @sysfs@ functions
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Useful low-level Linux @sysfs@ functions.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module System.GPIO.Linux.Sysfs.Util
       ( -- * Paths and file names
         sysfsPath
       , exportFileName
       , unexportFileName
       , pinDirName
       , pinActiveLowFileName
       , pinDirectionFileName
       , pinEdgeFileName
       , pinValueFileName
         -- * Convert Haskell types to/from their @sysfs@ representation
       , pinDirectionToBS
       , bsToPinDirection
       , sysfsEdgeToBS
       , bsToSysfsEdge
       , pinValueToBS
       , bsToPinValue
       , activeLowToBS
       , bsToActiveLow
       , intToBS
       , bsToInt
       ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (empty)
import Data.ByteString.Builder (toLazyByteString, intDec)
import qualified Data.ByteString.Char8 as C8 (readInt)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import System.FilePath ((</>))
import System.GPIO.Types (Pin(..), PinDirection(..), PinValue(..))
import System.GPIO.Linux.Sysfs.Types (SysfsEdge(..))

-- | The base path to Linux's @sysfs@ GPIO filesystem.
sysfsPath :: FilePath
sysfsPath = "/sys/class/gpio"

-- | The name of the control file used to export GPIO pins via
-- @sysfs@.
exportFileName :: FilePath
exportFileName = sysfsPath </> "export"

-- | The name of the control file used to "unexport" GPIO pins via
-- @sysfs@.
unexportFileName :: FilePath
unexportFileName = sysfsPath </> "unexport"

-- | Exporting a GPIO pin via @sysfs@ creates a control directory
-- corresponding to that pin. 'pinDirName' gives the name of that
-- directory for a given pin number.
pinDirName :: Pin -> FilePath
pinDirName (Pin n) = sysfsPath </> ("gpio" ++ show n)

-- | The name of the attribute file used to read and write the pin's
-- @active_low@ value.
pinActiveLowFileName :: Pin -> FilePath
pinActiveLowFileName p = pinDirName p </> "active_low"

-- | Pins whose direction can be controlled via @sysfs@ provide a
-- @direction@ attribute file. 'pinDirectionFileName' gives the name
-- of that file for a given pin number. Note that some pins' direction
-- cannot be set. In these cases, the file named by this function does
-- not actually exist.
pinDirectionFileName :: Pin -> FilePath
pinDirectionFileName p = pinDirName p </> "direction"

-- | Pins that can be configured as interrupt-generating inputs
-- provide an @edge@ attribute file. 'pinEdgeFileName' gives the name
-- of that file for a given pin number. Note that some pins' edge
-- configuration cannot be set. In these cases, the file named by this
-- function does not actually exist.
pinEdgeFileName :: Pin -> FilePath
pinEdgeFileName p = pinDirName p </> "edge"

-- | The name of the attribute file used to read and write the pin's
-- logical signal value.
pinValueFileName :: Pin -> FilePath
pinValueFileName p = pinDirName p </> "value"

-- | Convert a 'PinDirection' value to the corresponding 'ByteString'
-- value expected by a pin's @direction@ attribute in @sysfs@ GPIO
-- filesystem.
pinDirectionToBS :: PinDirection -> ByteString
pinDirectionToBS In = "in"
pinDirectionToBS Out = "out"

-- | When writing a pin's @direction@ attribute in the @sysfs@ GPIO
-- filesystem with a 'ByteString' value, @"in"@ configures the pin for
-- input, and @"out"@ configures the pin for output while also
-- initializing the pin's (logical) value as low.
--
-- Furthermore, you may write @"low"@ or @"high"@ to the @direction@
-- attribute to configure the pin for output and simulataneously set
-- the pin's (logical) value.
--
-- Therefore, writing a pin's @direction@ attribute potentially
-- affects both its direction and its value.
--
-- See the <https://www.kernel.org/doc/Documentation/gpio/sysfs.txt
-- Linux kernel documentation> for details.
--
-- This function converts a @direction@ attribute value, encoded as a
-- strict 'ByteString', to its corresponding 'PinDirection' and
-- 'PinValue' pair, or 'Nothing' if the attribute encoding is invalid.
bsToPinDirection :: ByteString -> Maybe (PinDirection, Maybe PinValue)
bsToPinDirection "in" = Just (In, Nothing)
bsToPinDirection "out" = Just (Out, Just Low)
bsToPinDirection "low" = Just (Out, Just Low)
bsToPinDirection "high" = Just (Out, Just High)
bsToPinDirection _ = Nothing

-- | Convert a 'SysfsEdge' value to the 'ByteString' value expected by
-- a pin's @edge@ attribute in the @sysfs@ GPIO filesystem.
sysfsEdgeToBS :: SysfsEdge -> ByteString
sysfsEdgeToBS None = "none"
sysfsEdgeToBS Rising = "rising"
sysfsEdgeToBS Falling = "falling"
sysfsEdgeToBS Both = "both"

-- | Inverse of 'sysfsEdgeToBS'.
bsToSysfsEdge :: ByteString -> Maybe SysfsEdge
bsToSysfsEdge "none" = Just None
bsToSysfsEdge "rising" = Just Rising
bsToSysfsEdge "falling" = Just Falling
bsToSysfsEdge "both" = Just Both
bsToSysfsEdge _ = Nothing

-- | Convert a 'PinValue' to the 'ByteString' value expected by a
-- pin's @value@ attribute in the @sysfs@ GPIO filesystem.
pinValueToBS :: PinValue -> ByteString
pinValueToBS Low = "0"
pinValueToBS High = "1"

-- | Convert a @value@ attribute value, encoded as a strict
-- 'ByteString', to its corresponding 'PinValue'.
--
-- Note that the @sysfs@ @value@ attribute is quite liberal: a
-- 'ByteString' value of "0" will set the pin's (logical) value to
-- low, but any other (non-empty) 'ByteString' value will set it to
-- high.
bsToPinValue :: ByteString -> Maybe PinValue
bsToPinValue "0" = Just Low
bsToPinValue bs
  | bs == BS.empty = Nothing
  | otherwise = Just High

-- | Convert a 'Bool' to the 'ByteString' value expected by a pin's
-- @active_low@ attribute in the @sysfs@ GPIO filesystem.
activeLowToBS :: Bool -> ByteString
activeLowToBS False = "0"
activeLowToBS True = "1"

-- | Convert an @active_low@ attribute value, encoded as a strict
-- 'ByteString', to its corresponding 'Bool' value.
--
-- Note that the @sysfs@ @active_low@ attribute is quite liberal: a
-- 'ByteString' value of "0" returns 'False' and any other (non-empty)
-- 'ByteString' value returns 'True'.
bsToActiveLow :: ByteString -> Maybe Bool
bsToActiveLow "0" = Just False
bsToActiveLow "1" = Just True
bsToActiveLow _ = Nothing

-- | Convert an 'Int' to a decimal ASCII encoding in a strict
-- 'ByteString'.
intToBS :: Int -> ByteString
intToBS = LBS.toStrict . toLazyByteString . intDec

-- | Convert a strict decimal ASCII 'ByteString' encoding of an
-- integer to an 'Int' (maybe). If there are any extraneous trailing
-- characters after the decimal ASCII encoding, other than a single
-- newline character, this is treated as a failure (unlike
-- 'C8.readInt', which returns the remaining string).
bsToInt :: ByteString -> Maybe Int
bsToInt = go . C8.readInt
  where
    go :: Maybe (Int, ByteString) -> Maybe Int
    go (Just (n, bs))
      | bs == BS.empty = Just n
      | bs == "\n" = Just n
      | otherwise = Nothing
    go _ = Nothing
