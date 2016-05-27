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
         --
         -- | A note on newlines: a Linux GPIO pin's /attributes/
         -- (i.e., the @sysfs@ files representing a pin's state) are
         -- read and written as 'ByteString's. When reading their
         -- contents, the attribute files always return their
         -- (ASCII-encoded) value followed by a newline character
         -- (@\\n@). When writing their contents, the attribute files
         -- will accept their (ASCII-encoded) new value either with or
         -- without a trailing newline character. For consistency (and
         -- for the sake of isomorphic conversions back-and-forth),
         -- these functions always use a trailing newline when
         -- encoding the ASCII value from the Haskell value.
       , pinDirectionToBS
       , pinDirectionValueToBS
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
--
-- >>> sysfsPath
-- "/sys/class/gpio"
sysfsPath :: FilePath
sysfsPath = "/sys/class/gpio"

-- | The name of the control file used to export GPIO pins via
-- @sysfs@.
--
-- >>> exportFileName
-- "/sys/class/gpio/export"
exportFileName :: FilePath
exportFileName = sysfsPath </> "export"

-- | The name of the control file used to "unexport" GPIO pins via
-- @sysfs@.
--
-- >>> unexportFileName
-- "/sys/class/gpio/unexport"
unexportFileName :: FilePath
unexportFileName = sysfsPath </> "unexport"

-- | Exporting a GPIO pin via @sysfs@ creates a control directory
-- corresponding to that pin. 'pinDirName' gives the name of that
-- directory for a given 'Pin'.
--
-- >>> pinDirName (Pin 16)
-- "/sys/class/gpio/gpio16"
pinDirName :: Pin -> FilePath
pinDirName (Pin n) = sysfsPath </> ("gpio" ++ show n)

-- | The name of the attribute file used to read and write the pin's
-- @active_low@ value.
--
-- >>> pinActiveLowFileName (Pin 16)
-- "/sys/class/gpio/gpio16/active_low"
pinActiveLowFileName :: Pin -> FilePath
pinActiveLowFileName p = pinDirName p </> "active_low"

-- | Pins whose direction can be controlled via @sysfs@ provide a
-- @direction@ attribute file. 'pinDirectionFileName' gives the name
-- of that file for a given 'Pin'. Note that some pins' direction
-- cannot be set. In these cases, the file named by this function does
-- not actually exist.
--
-- >>> pinDirectionFileName (Pin 16)
-- "/sys/class/gpio/gpio16/direction"
pinDirectionFileName :: Pin -> FilePath
pinDirectionFileName p = pinDirName p </> "direction"

-- | Pins that can be configured as interrupt-generating inputs
-- provide an @edge@ attribute file. 'pinEdgeFileName' gives the name
-- of that file for a given 'Pin'. Note that some pins' edge
-- configuration cannot be set. In these cases, the file named by this
-- function does not actually exist.
--
-- >>> pinEdgeFileName (Pin 16)
-- "/sys/class/gpio/gpio16/edge"
pinEdgeFileName :: Pin -> FilePath
pinEdgeFileName p = pinDirName p </> "edge"

-- | The name of the attribute file used to read and write the pin's
-- logical signal value.
--
-- >>> pinValueFileName (Pin 16)
-- "/sys/class/gpio/gpio16/value"
pinValueFileName :: Pin -> FilePath
pinValueFileName p = pinDirName p </> "value"

-- | Convert a 'PinDirection' value to the corresponding 'ByteString'
-- value expected by a pin's @direction@ attribute in the @sysfs@ GPIO
-- filesystem.
--
-- >>> pinDirectionToBS In
-- "in\n"
-- >>> pinDirectionToBS Out
-- "out\n"
pinDirectionToBS :: PinDirection -> ByteString
pinDirectionToBS In = "in\n"
pinDirectionToBS Out = "out\n"

-- | Convert a 'PinValue' value to the corresponding 'ByteString'
-- value expected by a pin's @direction@ attribute in the @sysfs@
-- GPIO, which can be used to configure the pin for output and
-- simultaneously set the pin's (physical) signal level; see the
-- <https://www.kernel.org/doc/Documentation/gpio/sysfs.txt Linux kernel documentation>
-- for details.
--
-- >>> pinDirectionValueToBS Low
-- "low\n"
-- >>> pinDirectionValueToBS High
-- "high\n"
pinDirectionValueToBS :: PinValue -> ByteString
pinDirectionValueToBS Low = "low\n"
pinDirectionValueToBS High = "high\n"

-- | When writing a pin's @direction@ attribute in the @sysfs@ GPIO
-- filesystem with a 'ByteString' value, @in\\n@ configures the pin
-- for input, and @out\\n@ configures the pin for output while also
-- initializing the pin's (physical) signal level to a low value.
--
-- Furthermore, you may write @low\\n@ or @high\\n@ to the
-- @direction@ attribute to configure the pin for output and
-- simulataneously set the pin's physical value.
--
-- Therefore, writing a pin's @direction@ attribute affects not only
-- its direction, but also (potentially) its value. This function's
-- return type reflects that possibility.
--
-- See the
-- <https://www.kernel.org/doc/Documentation/gpio/sysfs.txt Linux kernel documentation>
-- for details.
--
-- This function converts a @direction@ attribute value, encoded as a
-- strict 'ByteString', to its corresponding 'PinDirection' and
-- (possible) 'PinValue' pair; or 'Nothing' if the attribute encoding
-- is invalid.
--
-- >>> :set -XOverloadedStrings
-- >>> bsToPinDirection "in\n"
-- Just (In,Nothing)
-- >>> bsToPinDirection "out\n"
-- Just (Out,Just Low)
-- >>> bsToPinDirection "low\n"
-- Just (Out,Just Low)
-- >>> bsToPinDirection "high\n"
-- Just (Out,Just High)
-- >>> bsToPinDirection "foo\n"
-- Nothing
bsToPinDirection :: ByteString -> Maybe (PinDirection, Maybe PinValue)
bsToPinDirection "in\n" = Just (In, Nothing)
bsToPinDirection "out\n" = Just (Out, Just Low)
bsToPinDirection "low\n" = Just (Out, Just Low)
bsToPinDirection "high\n" = Just (Out, Just High)
bsToPinDirection _ = Nothing

-- | Convert a 'SysfsEdge' value to the 'ByteString' value expected by
-- a pin's @edge@ attribute in the @sysfs@ GPIO filesystem.
--
-- >>> sysfsEdgeToBS None
-- "none\n"
-- >>> sysfsEdgeToBS Rising
-- "rising\n"
-- >>> sysfsEdgeToBS Falling
-- "falling\n"
-- >>> sysfsEdgeToBS Both
-- "both\n"
sysfsEdgeToBS :: SysfsEdge -> ByteString
sysfsEdgeToBS None = "none\n"
sysfsEdgeToBS Rising = "rising\n"
sysfsEdgeToBS Falling = "falling\n"
sysfsEdgeToBS Both = "both\n"

-- | Inverse of 'sysfsEdgeToBS'.
--
-- >>> :set -XOverloadedStrings
-- >>> bsToSysfsEdge "none\n"
-- Just None
-- >>> bsToSysfsEdge "rising\n"
-- Just Rising
-- >>> bsToSysfsEdge "falling\n"
-- Just Falling
-- >>> bsToSysfsEdge "both\n"
-- Just Both
-- >>> bsToSysfsEdge "foo\n"
-- Nothing
bsToSysfsEdge :: ByteString -> Maybe SysfsEdge
bsToSysfsEdge "none\n" = Just None
bsToSysfsEdge "rising\n" = Just Rising
bsToSysfsEdge "falling\n" = Just Falling
bsToSysfsEdge "both\n" = Just Both
bsToSysfsEdge _ = Nothing

-- | Convert a 'PinValue' to the 'ByteString' value expected by a
-- pin's @value@ attribute in the @sysfs@ GPIO filesystem.
--
-- >>> pinValueToBS Low
-- "0\n"
-- >>> pinValueToBS High
-- "1\n"
pinValueToBS :: PinValue -> ByteString
pinValueToBS Low = "0\n"
pinValueToBS High = "1\n"

-- | Convert a @value@ attribute value, encoded as a strict
-- 'ByteString', to its corresponding 'PinValue'.
--
-- Note that the @sysfs@ @value@ attribute is quite liberal: a
-- 'ByteString' value of @0\\n@ will set the pin's (logical) signal
-- level to low, but any other (non-empty) 'ByteString' value will set
-- it to high.
--
-- >>> :set -XOverloadedStrings
-- >>> bsToPinValue "0\n"
-- Just Low
-- >>> bsToPinValue "1\n"
-- Just High
-- >>> bsToPinValue "high\n"
-- Just High
-- >>> bsToPinValue "low\n" -- nota bene!
-- Just High
-- >>> bsToPinValue "foo\n"
-- Just High
-- >>> bsToPinValue ""
-- Nothing
bsToPinValue :: ByteString -> Maybe PinValue
bsToPinValue "0\n" = Just Low
bsToPinValue bs
  | bs == BS.empty = Nothing
  | otherwise = Just High

-- | Convert a 'Bool' to the 'ByteString' value expected by a pin's
-- @active_low@ attribute in the @sysfs@ GPIO filesystem.
--
-- >>> activeLowToBS False
-- "0\n"
-- >>> activeLowToBS True
-- "1\n"
activeLowToBS :: Bool -> ByteString
activeLowToBS False = "0\n"
activeLowToBS True = "1\n"

-- | Convert an @active_low@ attribute value, encoded as a strict
-- 'ByteString', to its corresponding 'Bool' value.
--
-- Note that the @sysfs@ @active_low@ attribute is quite liberal: a
-- 'ByteString' value of @0\\n@ returns 'False' and any other
-- (non-empty) 'ByteString' value returns 'True'.
--
-- >>> :set -XOverloadedStrings
-- >>> bsToActiveLow "0\n"
-- Just False
-- >>> bsToActiveLow "1\n"
-- Just True
-- >>> bsToActiveLow "high\n"
-- Just True
-- >>> bsToActiveLow "low\n" -- nota bene!
-- Just True
-- >>> bsToActiveLow "foo\n"
-- Just True
-- >>> bsToActiveLow ""
-- Nothing
bsToActiveLow :: ByteString -> Maybe Bool
bsToActiveLow "0\n" = Just False
bsToActiveLow bs
  | bs == BS.empty = Nothing
  | otherwise = Just True

-- | Convert an 'Int' to a decimal ASCII encoding in a strict
-- 'ByteString'.
--
-- >>> intToBS 37
-- "37"
intToBS :: Int -> ByteString
intToBS = LBS.toStrict . toLazyByteString . intDec

-- | Convert a strict decimal ASCII 'ByteString' encoding of an
-- integer to an 'Int' (maybe). If there are any extraneous trailing
-- characters after the decimal ASCII encoding, other than a single
-- newline character, this is treated as a failure (unlike
-- 'C8.readInt', which returns the remaining string).
--
-- >>> :set -XOverloadedStrings
-- >>> bsToInt "37"
-- Just 37
-- >>> bsToInt "37\n"
-- Just 37
-- >>> bsToInt "37abc"
-- Nothing
-- >>> bsToInt "37 a"
-- Nothing
bsToInt :: ByteString -> Maybe Int
bsToInt = go . C8.readInt
  where
    go :: Maybe (Int, ByteString) -> Maybe Int
    go (Just (n, bs))
      | bs == BS.empty = Just n
      | bs == "\n" = Just n
      | otherwise = Nothing
    go _ = Nothing
