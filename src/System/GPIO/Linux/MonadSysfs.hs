-- | Monads in which low-level Linux 'sysfs' GPIO operations can be
-- embedded.
--
-- The chief purpose of this typeclass is to abstract the low-level
-- mechanisms from the 'sysfs' GPIO interface, so that a "mock"
-- implementation can be used for testing 'System.GPIO.Free.GpioF'
-- programs as if they were running on an actual Linux system with a
-- working 'sysfs'. Generally speaking, you should not use this
-- typeclass to write GPIO programs, and should instead use the
-- 'System.GPIO.Linux.Sysfs.runSysfsT' interpreter on
-- 'System.GPIO.Free.GpioF' programs so that your GPIO programs can
-- work portably across multiple GPIO implementations (and also
-- because the 'System.GPIO.Free.GpioF' has niceties like resource
-- bracketing for exception safety).
--
-- However, if you really want to write low-level, Linux-specific GPIO
-- programs without the overhead of the
-- 'Control.Monad.Trans.Free.FreeT' transformer, you can use this
-- typeclass directly. Do keep in mind, however, that this interface
-- may change at any time in order to suit the needs of the
-- 'System.GPIO.Free.GpioF' interpreter!

module System.GPIO.Linux.MonadSysfs
       ( -- * MonadSysfs class
         MonadSysfs(..)
        -- * Convenience functions
       , sysfsPath
       , exportFileName
       , unexportFileName
       , pinDirName
       , pinActiveLowFileName
       , pinDirectionFileName
       , pinEdgeFileName
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
import System.GPIO.Types

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

-- | The name of the control file used to read and write the pin's
-- "active low" value.
pinActiveLowFileName :: Pin -> FilePath
pinActiveLowFileName p = pinDirName p </> "active_low"

-- | Pins whose direction can be controlled via sysfs provide a
-- control file. 'pinDirectionFileName' gives the name of that file
-- for a given pin number. Note that some pins' direction cannot be
-- set. In these cases, the file named by this function does not
-- actually exist.
pinDirectionFileName :: Pin -> FilePath
pinDirectionFileName p = pinDirName p </> "direction"

-- | Pins that can be configured as interrupt-generating inputs
-- provide a control file. 'pinEdgeFileName' gives the name of that
-- file for a given pin number. Note that some pins' trigger
-- configuration cannot be set. In these cases, the file named by this
-- function does not actually exist.
pinEdgeFileName :: Pin -> FilePath
pinEdgeFileName p = pinDirName p </> "edge"

-- | The name of the control file used to read and write the pin's
-- value.
pinValueFileName :: Pin -> FilePath
pinValueFileName p = pinDirName p </> "value"


-- | Monads in which low-level Linux 'sysfs' GPIO-like operations may be
-- embedded.
--
-- Note that, in the spirit of Postel's law, functions which read
-- values from the 'sysfs' GPIO filesystem return a 'String' with the
-- raw 'sysfs' value returned by the kernel; but functions which write
-- values to 'sysfs' GPIO files take properly-typed values
-- corresponding to their 'System.GPIO.Free.GpioF' type. (This is
-- primarily done so that the conversion to/from 'String's need only
-- be done once in the 'System.GPIO.Linux.Sysfs.runSysfsT'
-- interpreter, instead of being needlessly repeated in each
-- 'MonadSysfs' implementation.)
class (Monad m) => MonadSysfs m where

  -- | Test whether the 'sysfs' GPIO filesystem is available.
  sysfsIsPresent :: m Bool

  -- | Test whether the given pin is already exported.
  pinIsExported :: Pin -> m Bool

  -- | Test whether the given pin's direction can be set.
  pinHasDirection :: Pin -> m Bool

  -- | Export the given pin.
  exportPin :: Pin -> m ()

  -- | Unexport the given pin.
  --
  -- It is an error to call this function if the pin is not currently
  -- exported.
  unexportPin :: Pin -> m ()

  -- | Read the given pin's direction as a 'String'. The returned
  -- value should be either "in\\n" or "out\\n" (note the trailing
  -- newline); any other value should be treated as an error.
  --
  -- It is an error to call this function if the pin has no
  -- user-visible direction in the 'sysfs' GPIO filesystem. (Some
  -- platforms may make the pin's direction available via other
  -- mechanisms, however.)
  readPinDirection :: Pin -> m String

  -- | Set the given pin's direction. Note that the function will
  -- convert the given 'PinDirection' value to the corresponding
  -- string value expected by the 'sysfs' GPIO filesystem.
  --
  -- It is an error to call this function if the pin has no
  -- user-visible direction in the 'sysfs' GPIO filesystem. (Some
  -- platforms may make the pin's direction settable via other
  -- mechanisms, however.)
  writePinDirection :: Pin -> PinDirection -> m ()

  -- | Pins whose direction can be set can be configured for output by
  -- writing a 'PinValue' to their sysfs "direction" attribute. This
  -- enables glitch-free output configuration, assuming the pin is
  -- currently configured for input, or some kind of tri-stated or
  -- floating high-impedance mode.
  --
  -- It is an error to call this function if the pin has no
  -- user-visible direction in the 'sysfs' GPIO filesystem. (Some
  -- platforms may make the pin's direction settable via other
  -- mechanisms, however.)
  writePinDirectionWithValue :: Pin -> PinValue -> m ()

  -- | Read the given pin's value as a 'String'. The returned value
  -- should be either "0\\n" or "1\\n" (note the trailing newline); any
  -- other value should be treated as an error.
  --
  -- Note that this function never blocks, regardless of the pin's
  -- 'PinReadTrigger' setting.
  readPinValue :: Pin -> m String

  -- | A blocking version of 'readPinValue'. The current thread will
  -- block until an event occurs on the pin as specified by the pin's
  -- current 'PinReadTrigger' setting.
  --
  -- If your program is compiled with GHC using the @-threaded@
  -- option, this function will not block other Haskell threads.
  threadWaitReadPinValue :: Pin -> m String

  -- | Set the given pin's value. Note that the function will convert
  -- the given 'PinValue' value to the corresponding string value
  -- expected by the 'sysfs' GPIO filesystem.
  --
  -- It is an error to call this function if the pin is configured as
  -- an input pin.
  writePinValue :: Pin -> PinValue -> m ()

  -- | Test whether the pin has an "edge" 'sysfs' attribute, i.e.,
  -- whether it can be configured for edge- or level-triggered
  -- interrupts.
  pinHasEdge :: Pin -> m Bool

  -- | Read the given pin's "edge" 'sysfs' attribute as a 'String'.
  -- The returned value should be one of "none\\n", "rising\\n",
  -- "falling\\n", or "both\\n" (note the trailing newline). Any other
  -- values should be treated as an error.
  --
  -- It is an error to call this function when the pin has no "edge"
  -- attribute.
  readPinEdge :: Pin -> m String

  -- | Write the given pin's "edge" 'sysfs' attribute.
  --
  -- It is an error to call this function when the pin has no "edge"
  -- attribute.
  writePinEdge :: Pin -> PinReadTrigger -> m ()

  -- | Read the given pin's "active_low" 'sysfs' attribute as a
  -- 'String'. The returned value should be either "0\\n" or "1\\n"
  -- (note the trailing newline). Any other values should be treated
  -- as an error.
  readPinActiveLow :: Pin -> m String

  -- | Write the given pin's "active_low" 'sysfs' attribute.
  writePinActiveLow :: Pin -> Bool -> m ()

  -- | Return a list of all pins that are exposed via the 'sysfs' GPIO
  -- filesystem. Note that the returned list may omit some pins that
  -- are available on the host but which, for various reasons, are not
  -- exposed via the 'sysfs' GPIO interface.
  availablePins :: m [Pin]

instance (MonadIO m, MonadSysfs m) => MonadSysfs (ContT r m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  threadWaitReadPinValue = lift . threadWaitReadPinValue
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (ExceptT e m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  threadWaitReadPinValue = lift . threadWaitReadPinValue
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (ListT m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  threadWaitReadPinValue = lift . threadWaitReadPinValue
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (MaybeT m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  threadWaitReadPinValue = lift . threadWaitReadPinValue
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (ReaderT r m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  threadWaitReadPinValue = lift . threadWaitReadPinValue
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m, Monoid w) => MonadSysfs (LazyRWS.RWST r w s m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  threadWaitReadPinValue = lift . threadWaitReadPinValue
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m, Monoid w) => MonadSysfs (StrictRWS.RWST r w s m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  threadWaitReadPinValue = lift . threadWaitReadPinValue
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (LazyState.StateT s m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  threadWaitReadPinValue = lift . threadWaitReadPinValue
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m) => MonadSysfs (StrictState.StateT s m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  threadWaitReadPinValue = lift . threadWaitReadPinValue
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m, Monoid w) => MonadSysfs (LazyWriter.WriterT w m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  threadWaitReadPinValue = lift . threadWaitReadPinValue
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadIO m, MonadSysfs m, Monoid w) => MonadSysfs (StrictWriter.WriterT w m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  unexportPin = lift . unexportPin
  readPinDirection = lift . readPinDirection
  writePinDirection h d = lift $ writePinDirection h d
  writePinDirectionWithValue h v = lift $ writePinDirectionWithValue h v
  readPinValue = lift . readPinValue
  threadWaitReadPinValue = lift . threadWaitReadPinValue
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins
