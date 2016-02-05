-- | A monad type class for Linux 'sysfs' GPIO operations.

module System.GPIO.Linux.MonadSysfs
       ( -- * MonadSysfs class
         MonadSysfs(..)
       ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)
import System.GPIO.Types
import System.GPIO.Linux.SysfsTypes

-- | A type class for monads which implement (or mock) low-level Linux
-- 'sysfs' GPIO operations.
--
-- This type class chiefly exists in order to use a 'sysfs' mock for
-- testing with the 'System.GPIO.Linux.Sysfs.SysfsT'
-- transformer/interpreter (see
-- 'System.GPIO.Linux.SysfsMock.SysfsMockT'). Typically, you will not
-- need to use it directly. If you want to write Linux 'sysfs' GPIO
-- programs that run in 'IO', see the 'System.GPIO.Linux.SysfsIO'
-- module.
class (Monad m) => MonadSysfs m where

  -- | Test whether the 'sysfs' GPIO filesystem is available.
  sysfsIsPresent :: m Bool

  -- | Test whether the given pin is already exported.
  pinIsExported :: Pin -> m Bool

  -- | Test whether the given pin's direction can be set via the
  -- 'sysfs' GPIO filesystem. (Some pins have a hard-wired direction,
  -- in which case their direction must be determined by some other
  -- mechanism as the "direction" attribute does not exist for such
  -- pins.)
  pinHasDirection :: Pin -> m Bool

  -- | Export the given pin.
  exportPin :: Pin -> m ()

  -- | Unexport the given pin.
  --
  -- It is an error to call this function if the pin is not currently
  -- exported.
  unexportPin :: Pin -> m ()

  -- | Read the given pin's direction.
  --
  -- It is an error to call this function if the pin has no
  -- "direction" attribute in the 'sysfs' GPIO filesystem.
  readPinDirection :: Pin -> m PinDirection

  -- | Set the given pin's direction.
  --
  -- It is an error to call this function if the pin has no
  -- "direction" attribute in the 'sysfs' GPIO filesystem.
  writePinDirection :: Pin -> PinDirection -> m ()

  -- | Pins whose direction can be set may be configured for output by
  -- writing a 'PinValue' to their 'sysfs' "direction" attribute. This
  -- enables glitch-free output configuration, assuming the pin is
  -- currently configured for input, or some kind of tri-stated or
  -- floating high-impedance mode.
  --
  -- It is an error to call this function if the pin has no
  -- "direction" attribute in the 'sysfs' GPIO filesystem.
  writePinDirectionWithValue :: Pin -> PinValue -> m ()

  -- | Read the given pin's value.
  --
  -- Note that this function never blocks, regardless of the pin's
  -- "edge" attribute setting.
  readPinValue :: Pin -> m PinValue

  -- | A blocking version of 'readPinValue'. The current thread will
  -- block until an event occurs on the pin as specified by the pin's
  -- current "edge" attribute setting.
  --
  -- If the pin has no "edge" attribute, then this function will not
  -- block and will act like 'readPinValue'.
  threadWaitReadPinValue :: Pin -> m PinValue

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
  -- If the pin has no "edge" attribute, then this function will not
  -- block and will act like 'readPinValue'.
  threadWaitReadPinValue' :: Pin -> Int -> m (Maybe PinValue)

  -- | Set the given pin's value.
  --
  -- It is an error to call this function if the pin is configured as
  -- an input pin.
  writePinValue :: Pin -> PinValue -> m ()

  -- | Test whether the pin has an "edge" 'sysfs' attribute, i.e.,
  -- whether it can be configured for edge- or level-triggered
  -- interrupts.
  pinHasEdge :: Pin -> m Bool

  -- | Read the given pin's "edge" 'sysfs' attribute.
  --
  -- It is an error to call this function when the pin has no "edge"
  -- attribute.
  readPinEdge :: Pin -> m SysfsEdge

  -- | Write the given pin's "edge" 'sysfs' attribute.
  --
  -- It is an error to call this function when the pin has no "edge"
  -- attribute.
  writePinEdge :: Pin -> SysfsEdge -> m ()

  -- | Read the given pin's "active_low" 'sysfs' attribute.
  readPinActiveLow :: Pin -> m Bool

  -- | Write the given pin's "active_low" 'sysfs' attribute.
  writePinActiveLow :: Pin -> Bool -> m ()

  -- | Return a list of all pins that are exposed via the 'sysfs' GPIO
  -- filesystem. Note that the returned list may omit some pins that
  -- are available on the host but which, for various reasons, are not
  -- exposed via the 'sysfs' GPIO filesystem.
  availablePins :: m [Pin]

instance (MonadSysfs m) => MonadSysfs (IdentityT m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadSysfs m) => MonadSysfs (ContT r m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadSysfs m) => MonadSysfs (ExceptT e m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadSysfs m) => MonadSysfs (ListT m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadSysfs m) => MonadSysfs (MaybeT m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadSysfs m) => MonadSysfs (ReaderT r m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadSysfs m, Monoid w) => MonadSysfs (LazyRWS.RWST r w s m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadSysfs m, Monoid w) => MonadSysfs (StrictRWS.RWST r w s m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadSysfs m) => MonadSysfs (LazyState.StateT s m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadSysfs m) => MonadSysfs (StrictState.StateT s m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadSysfs m, Monoid w) => MonadSysfs (LazyWriter.WriterT w m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins

instance (MonadSysfs m, Monoid w) => MonadSysfs (StrictWriter.WriterT w m) where
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
  threadWaitReadPinValue' p timeout = lift $ threadWaitReadPinValue' p timeout
  writePinValue h v = lift $ writePinValue h v
  readPinEdge = lift . readPinEdge
  writePinEdge h x = lift $ writePinEdge h x
  readPinActiveLow = lift . readPinActiveLow
  writePinActiveLow h v = lift $ writePinActiveLow h v
  availablePins = lift availablePins
