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

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)
import System.FilePath ((</>))
import System.GPIO.Free (PinDirection(..), Pin(..), PinValue(..))

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

instance (MonadIO m, MonadSysfs m) => MonadSysfs (ContT r m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  writePinValue h v = lift $ writePinValue h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (ExceptT e m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  writePinValue h v = lift $ writePinValue h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (ListT m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  writePinValue h v = lift $ writePinValue h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (MaybeT m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  writePinValue h v = lift $ writePinValue h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (ReaderT r m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  writePinValue h v = lift $ writePinValue h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m, Monoid w) => MonadSysfs (LazyRWS.RWST r w s m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  writePinValue h v = lift $ writePinValue h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m, Monoid w) => MonadSysfs (StrictRWS.RWST r w s m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  writePinValue h v = lift $ writePinValue h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (LazyState.StateT s m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  writePinValue h v = lift $ writePinValue h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (StrictState.StateT s m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  writePinValue h v = lift $ writePinValue h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m, Monoid w) => MonadSysfs (LazyWriter.WriterT w m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  writePinValue h v = lift $ writePinValue h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m, Monoid w) => MonadSysfs (StrictWriter.WriterT w m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  writePinValue h v = lift $ writePinValue h v
  availablePins = lift availablePins

