-- | The actual Linux 'sysfs' GPIO implementation.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.GPIO.Linux.SysfsIO
         ( -- * 'SysfsIOT' transformer
           SysfsIOT(..)
         , SysfsIO
         , Sysfs
         , runSysfs
         , runSysfsSafe
         , runSysfs'
         ) where

import Prelude hiding (readFile, writeFile)
import Control.Applicative
import Control.Error.Script (scriptIO)
import Control.Error.Util (hushT)
import Control.Monad (filterM)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Char (toLower)
import Data.List (isPrefixOf, sort)
import Data.Maybe (catMaybes)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName)
import System.GPIO.Free (PinDirection(..), Pin(..), PinValue(..))
import System.GPIO.Linux.MonadSysfs
import System.GPIO.Linux.Sysfs
import qualified System.IO as IO (writeFile)
import qualified System.IO.Strict as IOS (readFile)

newtype SysfsIOT m a =
  SysfsIOT { runSysfsIOT :: m a }
  deriving (Alternative,Applicative,Functor,Monad,MonadIO)

-- | A convenient specialization of 'SysfsIOT' which runs computations
-- in the 'IO' monad.
type SysfsIO a = SysfsIOT IO a

-- | A convenient specialization of 'SysfsT' which runs GPIO
-- computations in the 'SysfsIO' monad, and returns results as
-- 'Either' 'String' 'a'.
type Sysfs a = SysfsT (ExceptT String (SysfsIOT IO)) (ExceptT String (SysfsIOT IO)) a

-- | Run a 'Sysfs' computation in the 'IO' monad and return the result
-- as 'Right' 'a'. If an error occurs in the computation or in the
-- interpreter, it is returned as 'Left' 'String'. However, the
-- function does not catch any 'Control.Exception.Base.IOException's
-- that occur as a side effect of the computation; those will
-- propagate upwards.
runSysfs :: Sysfs a -> IO (Either String a)
runSysfs action = runSysfsIOT $ runExceptT $ runSysfsT action

-- | Run a 'Sysfs' computation and return the result in as 'Right'
-- 'a'. If an error in the computation, in the 'Sysfs' interpreter, or
-- elsewhere while the computation is running (including
-- 'Control.Exception.Base.IOException's that occur as a side effect
-- of the computation) it is handled by this function and is returned
-- as an error result via 'Left' 'String'. Therefore, this function is
-- total; i.e., it always returns a result (assuming the computation
-- terminates) and never throws an exception.
runSysfsSafe :: Sysfs a -> IO (Either String a)
runSysfsSafe action =
  do result <- runExceptT $ scriptIO (runSysfs action)
     case result of
       Left e -> return $ Left e
       Right a -> return a

-- | Run a 'Sysfs' computation in the 'IO' monad and return the
-- result. If an error occurs in the computation, in the 'Sysfs'
-- interpreter, or in the "real world" (e.g., 'IO' exceptions) it will
-- be thrown as an 'Control.Exception.Base.IOException'. In other
-- words, all errors will be expressed as
-- 'Control.Exception.Base.IOException's, just as a plain 'IO'
-- computation would do.
runSysfs' :: Sysfs a -> IO a
runSysfs' action =
  do result <- runSysfs action
     case result of
       Left e -> fail e
       Right a -> return a

instance (MonadIO m) => MonadSysfs (SysfsIOT m) where
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
