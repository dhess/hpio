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
       ( sysfsPath
       , exportFileName
       , unexportFileName
       , pinDirName
       , pinActiveLowFileName
       , pinDirectionFileName
       , pinEdgeFileName
       , pinValueFileName
       , intToByteString
       , byteStringToInt
       ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (empty)
import Data.ByteString.Builder (toLazyByteString, intDec)
import qualified Data.ByteString.Char8 as C8 (readInt)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import System.FilePath ((</>))
import System.GPIO.Types (Pin(..))

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

-- | Convert an 'Int' to a decimal ASCII encoding in a strict
-- 'ByteString'.
intToByteString :: Int -> ByteString
intToByteString = LBS.toStrict . toLazyByteString . intDec

-- | Convert a strict decimal ASCII 'ByteString' encoding of an
-- integer to an 'Int' (maybe). If there are any extraneous trailing
-- characters after the decimal ASCII encoding, other than a single
-- newline character, this is treated as a failure (unlike
-- 'C8.readInt', which returns the remaining string).
byteStringToInt :: ByteString -> Maybe Int
byteStringToInt = go . C8.readInt
  where
    go :: Maybe (Int, ByteString) -> Maybe Int
    go (Just (n, bs))
      | bs == BS.empty = Just n
      | bs == "\n" = Just n
      | otherwise = Nothing
    go _ = Nothing
