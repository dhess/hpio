{-|
Module      : System.GPIO.Linux.Sysfs.Native
Description : Native Linux @sysfs@ GPIO
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Functions for performing native GPIO operations using the Linux
@sysfs@ filesystem.

Note that these functions do not run directly in the 'System.IO.IO'
monad; they run in the 'MonadSysfs' monad. We use this abstraction
primarily to allow for testing/mocking of the @sysfs@ filesystem API
without needing an actual @sysfs@ filesystem to run against.

See the "System.GPIO.Linux.Sysfs.IO" module for an instance of
'MonadSysfs' which runs the actual @sysfs@ actions in 'System.IO.IO'.

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module System.GPIO.Linux.Sysfs.Native
       ( -- * Low-level @sysfs@ GPIO functions.
         sysfsIsPresent
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
       ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad (filterM)
import Control.Monad.Catch
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8 (readInt, unpack)
import Data.List (isPrefixOf, sort)
import System.FilePath ((</>), takeFileName)
import System.GPIO.Linux.Sysfs.Monad (MonadSysfs(..))
import System.GPIO.Linux.Sysfs.Types
import System.GPIO.Linux.Sysfs.Util
import System.GPIO.Types

-- | Test whether the @sysfs@ GPIO filesystem is available.
sysfsIsPresent :: (MonadSysfs m) => m Bool
sysfsIsPresent = doesDirectoryExist sysfsPath

-- | Test whether the given pin is already exported.
pinIsExported :: (MonadSysfs m) => Pin -> m Bool
pinIsExported = doesDirectoryExist . pinDirName

-- | Export the given pin.
--
-- Note that it's an error to call this function to export a pin
-- that's already been exported.
exportPin :: (MonadSysfs m) => Pin -> m ()
exportPin (Pin n) = unlockedWriteFile exportFileName (intToByteString n)

-- | Export the given pin, but, unlike 'exportPin', if the pin is
-- already exported, this is not an error; in this situation, the pin
-- remains exported and its state unchanged.
exportPin' :: (MonadSysfs m) => Pin -> m ()
exportPin' p =
  pinIsExported p >>= \case
    True -> return ()
    False -> exportPin p

-- | Unexport the given pin.
--
-- It is an error to call this function if the pin is not currently
-- exported.
unexportPin :: (MonadSysfs m) => Pin -> m ()
unexportPin (Pin n) = unlockedWriteFile unexportFileName (intToByteString n)

-- | Test whether the given pin's direction can be set via the
-- @sysfs@ GPIO filesystem. (Some pins have a hard-wired direction,
-- in which case their direction must be determined by some other
-- mechanism as the @direction@ attribute does not exist for such
-- pins.)
pinHasDirection :: (MonadSysfs m) => Pin -> m Bool
pinHasDirection = doesFileExist . pinDirectionFileName

-- | Read the given pin's direction.
--
-- It is an error to call this function if the pin has no @direction@
-- attribute.
readPinDirection :: (MonadSysfs m, MonadThrow m) => Pin -> m PinDirection
readPinDirection p =
  readFile (pinDirectionFileName p) >>= \case
    "in\n"  -> return In
    "out\n" -> return Out
    x     -> throwM $ UnexpectedDirection p (C8.unpack x)

-- | Set the given pin's direction.
--
-- It is an error to call this function if the pin has no @direction@
-- attribute.
writePinDirection :: (MonadSysfs m) => Pin -> PinDirection -> m ()
writePinDirection p d = writeFile (pinDirectionFileName p) (toSysfsPinDirection d)

-- | Pins whose direction can be set may be configured for output by
-- writing a 'PinValue' to their @direction@ attribute. This enables
-- glitch-free output configuration, assuming the pin is currently
-- configured for input, or some kind of tri-stated or floating
-- high-impedance mode.
--
-- It is an error to call this function if the pin has no
-- @direction@ attribute.
writePinDirectionWithValue :: (MonadSysfs m) => Pin -> PinValue -> m ()
writePinDirectionWithValue p v = writeFile (pinDirectionFileName p) (valueToPinDirection v)

-- | Read the given pin's value.
--
-- Note that this function never blocks, regardless of the pin's
-- @edge@ attribute setting.
readPinValue :: (MonadSysfs m, MonadThrow m) => Pin -> m PinValue
readPinValue p =
  readFile (pinValueFileName p) >>= \case
    "0\n" -> return Low
    "1\n" -> return High
    x   -> throwM $ UnexpectedValue p (C8.unpack x)

-- | A blocking version of 'readPinValue'. The current thread will
-- block until an event occurs on the pin as specified by the pin's
-- current @edge@ attribute setting.
--
-- If the pin has no @edge@ attribute, then this function will not
-- block and will act like 'readPinValue'.
threadWaitReadPinValue :: (Functor m, MonadSysfs m, MonadThrow m) => Pin -> m PinValue
threadWaitReadPinValue p =
  threadWaitReadPinValue' p (-1) >>= \case
    Just v -> return v
    -- Yes, I really do mean "error" here. 'Nothing' can only occur
    -- when the poll has timed out, but the (-1) timeout value above
    -- means the poll must either wait forever or fail.
    Nothing -> error "threadWaitReadPinValue timed out, and it should not have. Please file a bug at https://github.com/dhess/gpio"

-- | Same as 'threadWaitReadPinValue', except that a timeout value,
-- specified in microseconds, is provided. If no event occurs before
-- the timeout expires, this function returns 'Nothing'; otherwise,
-- it returns the pin's value wrapped in a 'Just'.
--
-- If the timeout value is negative, this function behaves just like
-- 'threadWaitReadPinValue'.
--
-- When specifying a timeout value, be careful not to exceed
-- 'maxBound'.
--
-- If the pin has no @edge@ attribute, then this function will not
-- block and will act like 'readPinValue'.
threadWaitReadPinValue' :: (Functor m, MonadSysfs m, MonadThrow m) => Pin -> Int -> m (Maybe PinValue)
threadWaitReadPinValue' p timeout =
  do pollResult <- pollFile (pinValueFileName p) timeout
     if pollResult > 0
       then Just <$> readPinValue p
       else return Nothing

-- | Set the given pin's value.
--
-- It is an error to call this function if the pin is configured as
-- an input pin.
writePinValue :: (MonadSysfs m) => Pin -> PinValue -> m ()
writePinValue p v = writeFile (pinValueFileName p) (toSysfsPinValue v)

-- | Test whether the pin has an @edge@ attribute, i.e., whether it
-- can be configured for edge- or level-triggered interrupts.
pinHasEdge :: (MonadSysfs m) => Pin -> m Bool
pinHasEdge p = doesFileExist (pinEdgeFileName p)

-- | Read the given pin's @edge@ attribute.
--
-- It is an error to call this function when the pin has no @edge@
-- attribute.
readPinEdge :: (MonadSysfs m, MonadThrow m) => Pin -> m SysfsEdge
readPinEdge p =
  readFile (pinEdgeFileName p) >>= \case
    "none\n"  -> return None
    "rising\n" -> return Rising
    "falling\n" -> return Falling
    "both\n" -> return Both
    x     -> throwM $ UnexpectedEdge p (C8.unpack x)

-- | Write the given pin's @edge@ attribute.
--
-- It is an error to call this function when the pin has no @edge@
-- attribute.
writePinEdge :: (MonadSysfs m) => Pin -> SysfsEdge -> m ()
writePinEdge p v = writeFile (pinEdgeFileName p) (toSysfsPinEdge v)

-- | Read the given pin's @active_low@ attribute.
readPinActiveLow :: (MonadSysfs m, MonadThrow m) => Pin -> m Bool
readPinActiveLow p =
  readFile (pinActiveLowFileName p) >>= \case
    "0\n" -> return False
    "1\n" -> return True
    x   -> throwM $ UnexpectedActiveLow p (C8.unpack x)

-- | Write the given pin's @active_low@ attribute.
writePinActiveLow :: (MonadSysfs m) => Pin -> Bool -> m ()
writePinActiveLow p v = writeFile (pinActiveLowFileName p) (toSysfsActiveLowValue v)

-- | Return a list of all pins that are exposed via the @sysfs@ GPIO
-- filesystem. Note that the returned list may omit some pins that
-- are available on the host but which, for various reasons, are not
-- exposed via the @sysfs@ GPIO filesystem.
availablePins :: (MonadSysfs m, MonadThrow m) => m [Pin]
availablePins =
  do sysfsEntries <- getDirectoryContents sysfsPath
     let sysfsContents = fmap (sysfsPath </>) sysfsEntries
     sysfsDirectories <- filterM doesDirectoryExist sysfsContents
     let chipDirs = filter (isPrefixOf "gpiochip" . takeFileName) sysfsDirectories
     pins <- mapM pinRange chipDirs
     return $ sort $ concat pins

-- Helper functions that aren't exported.
--

toSysfsPinDirection :: PinDirection -> ByteString
toSysfsPinDirection In = "in"
toSysfsPinDirection Out = "out"

toSysfsPinEdge :: SysfsEdge -> ByteString
toSysfsPinEdge None = "none"
toSysfsPinEdge Rising = "rising"
toSysfsPinEdge Falling = "falling"
toSysfsPinEdge Both = "both"

toSysfsPinValue :: PinValue -> ByteString
toSysfsPinValue Low = "0"
toSysfsPinValue High = "1"

toSysfsActiveLowValue :: Bool -> ByteString
toSysfsActiveLowValue False = "0"
toSysfsActiveLowValue True = "1"

-- Used for writing pin direction with a value.
valueToPinDirection :: PinValue -> ByteString
valueToPinDirection Low = "low"
valueToPinDirection High = "high"

readIntFromFile :: (MonadSysfs m, MonadThrow m) => FilePath -> m Int
readIntFromFile f =
  do contents <- readFile f
     case C8.readInt contents of
       Just (n, _) -> return n
       Nothing -> throwM $ UnexpectedContents f (C8.unpack contents)

pinRange :: (MonadSysfs m, MonadThrow m) => FilePath -> m [Pin]
pinRange chipDir =
  do base <- readIntFromFile (chipDir </> "base")
     ngpio <- readIntFromFile (chipDir </> "ngpio")
     if base >= 0 && ngpio > 0
        then return $ fmap Pin [base .. (base + ngpio - 1)]
        else return []
