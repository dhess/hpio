-- | Monads in which low-level Linux 'sysfs' GPIO operations can be
-- embedded.

module System.GPIO.Linux.MonadSysfs
       ( -- * MonadSysfs class
         MonadSysfs(..)
        -- * Convenience functions
       , sysfsPath
       , exportFileName
       , unexportFileName
       , pinDirName
       , pinDirectionFileName
       , pinValueFileName
       ) where

import Prelude hiding (readFile, writeFile)
import Control.Error.Script (scriptIO)
import Control.Error.Util (hushT)
import Control.Monad (filterM)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Char (toLower)
import Data.List (isPrefixOf, sort)
import Data.Maybe (catMaybes)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName)
import System.GPIO.Free (PinDirection(..), Pin(..), PinValue(..))
import qualified System.IO as IO (writeFile)
import qualified System.IO.Strict as IOS (readFile)

-- | The base path to Linux's GPIO sysfs interface.
sysfsPath :: FilePath
sysfsPath = "/sys/class/gpio"

-- | The name of the control file used to export GPIO pins via sysfs.
exportFileName :: FilePath
exportFileName = sysfsPath </> "export"

-- | The name of the control file used to "unexport" GPIO pins via
-- sysfs.
unexportFileName :: FilePath
unexportFileName = sysfsPath </> "unexport"

-- | Exporting a GPIO pin via sysfs creates a control directory
-- corresponding to that pin. 'pinDirName' gives the name of that
-- directory for a given pin number.
pinDirName :: Pin -> FilePath
pinDirName (Pin n) = sysfsPath </> ("gpio" ++ show n)

-- | Pins whose direction can be controlled via sysfs provide a
-- control file. 'pinDirectionFileName' gives the name of that file
-- for a given pin number. Note that some pins' direction cannot be
-- set. In these cases, the file named by this function does not
-- actually exist.
pinDirectionFileName :: Pin -> FilePath
pinDirectionFileName p = pinDirName p </> "direction"

-- | The name of the control file used to read and write the pin's
-- value.
pinValueFileName :: Pin -> FilePath
pinValueFileName p = pinDirName p </> "value"

-- | Monads in which low-level Linux 'sysfs' GPIO-like operations may be
-- embedded.
--
-- The purpose of this typeclass is to abstract the low-level
-- mechanisms from the interface, chiefly so that a "mock"
-- implementation can be used for testing 'GpioF' programs as if they
-- were running on an actual Linux system with a working 'sysfs'.
class (Monad m) => MonadSysfs m where
  sysfsIsPresent :: m Bool
  pinIsExported :: Pin -> m Bool
  pinHasDirection :: Pin -> m Bool
  exportPin :: Pin -> m ()
  unexportPin :: Pin -> m ()
  readPinDirection :: Pin -> m String
  writePinDirection :: Pin -> PinDirection -> m ()
  writePinDirectionWithValue :: Pin -> PinValue -> m ()
  readPinValue :: Pin -> m String
  writePinValue :: Pin -> PinValue -> m ()
  availablePins :: m [Pin]

instance MonadSysfs IO where
  sysfsIsPresent = sysfsIsPresentIO
  pinIsExported = pinIsExportedIO
  pinHasDirection = pinHasDirectionIO
  exportPin = exportPinIO
  unexportPin = unexportPinIO
  readPinDirection = readPinDirectionIO
  writePinDirection = writePinDirectionIO
  writePinDirectionWithValue = writePinDirectionWithValueIO
  readPinValue = readPinValueIO
  writePinValue = writePinValueIO
  availablePins = availablePinsIO

instance (MonadIO m, MonadSysfs m) => MonadSysfs (ExceptT e m) where
  sysfsIsPresent = sysfsIsPresentIO
  pinIsExported = pinIsExportedIO
  pinHasDirection = pinHasDirectionIO
  exportPin = exportPinIO
  unexportPin = unexportPinIO
  readPinDirection = readPinDirectionIO
  writePinDirection = writePinDirectionIO
  writePinDirectionWithValue = writePinDirectionWithValueIO
  readPinValue = readPinValueIO
  writePinValue = writePinValueIO
  availablePins = availablePinsIO

-- Helper functions that aren't exported.
--

sysfsIsPresentIO :: (MonadIO m) => m Bool
sysfsIsPresentIO = liftIO $ doesDirectoryExist sysfsPath

pinIsExportedIO :: (MonadIO m) => Pin -> m Bool
pinIsExportedIO p = liftIO $ doesDirectoryExist (pinDirName p)

pinHasDirectionIO :: (MonadIO m) => Pin -> m Bool
pinHasDirectionIO p = liftIO $ doesFileExist (pinDirectionFileName p)

exportPinIO :: (MonadIO m) => Pin -> m ()
exportPinIO (Pin n) = liftIO $ IO.writeFile exportFileName (show n)

unexportPinIO :: (MonadIO m) => Pin -> m ()
unexportPinIO (Pin n) = liftIO $ IO.writeFile unexportFileName (show n)

readPinDirectionIO :: (MonadIO m) => Pin -> m String
readPinDirectionIO p = liftIO $ IOS.readFile (pinDirectionFileName p)

writePinDirectionIO :: (MonadIO m) => Pin -> PinDirection -> m ()
writePinDirectionIO p d = liftIO $ IO.writeFile (pinDirectionFileName p) (lowercase $ show d)

writePinDirectionWithValueIO :: (MonadIO m) => Pin -> PinValue -> m ()
writePinDirectionWithValueIO p v = liftIO $ IO.writeFile (pinDirectionFileName p) (lowercase $ show v)

readPinValueIO :: (MonadIO m) => Pin -> m String
readPinValueIO p = liftIO $ IOS.readFile (pinValueFileName p)

writePinValueIO :: (MonadIO m) => Pin -> PinValue -> m ()
writePinValueIO p v = liftIO $ IO.writeFile (pinValueFileName p) (toSysfsPinValue v)

availablePinsIO :: (MonadIO m) => m [Pin]
availablePinsIO =
  do sysfsEntries <- liftIO $ getDirectoryContents sysfsPath
     let sysfsContents = fmap (sysfsPath </>) sysfsEntries
     sysfsDirectories <- filterM (liftIO . doesDirectoryExist) sysfsContents
     let chipDirs = filter (\f -> isPrefixOf "gpiochip" $ takeFileName f) sysfsDirectories
     maybePins <- mapM (runMaybeT . pinRange) chipDirs
     return $ sort $ concat $ catMaybes maybePins

lowercase :: String -> String
lowercase = fmap toLower

toSysfsPinValue :: PinValue -> String
toSysfsPinValue Low = "0"
toSysfsPinValue High = "1"

readFromFile :: (MonadIO m, Read a) => FilePath -> m a
readFromFile f = liftIO (IOS.readFile f >>= readIO)

maybeIO :: (MonadIO m) => IO a -> MaybeT m a
maybeIO = hushT . scriptIO

chipBaseGpio :: (MonadIO m) => FilePath -> m Int
chipBaseGpio chipDir = readFromFile (chipDir </> "base")

chipNGpio :: (MonadIO m) => FilePath -> m Int
chipNGpio chipDir = readFromFile (chipDir </> "ngpio")

pinRange :: (MonadIO m) => FilePath -> MaybeT m [Pin]
pinRange chipDir =
  do base <- maybeIO $ chipBaseGpio chipDir
     ngpio <- maybeIO $ chipNGpio chipDir
     case (base >= 0 && ngpio > 0) of
       False -> return []
       True -> return $ fmap Pin [base .. (base + ngpio - 1)]
