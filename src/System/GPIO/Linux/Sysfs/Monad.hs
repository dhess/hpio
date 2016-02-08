{-|
Module      : System.GPIO.Linux.Sysfs.Monad
Description : A monad type class for Linux 'sysfs' GPIO operations
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

A monad type class for Linux 'sysfs' GPIO operations.

-}

{-# LANGUAGE Safe #-}

module System.GPIO.Linux.Sysfs.Monad
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
import System.GPIO.Linux.Sysfs.Types

-- | A type class for monads which implement (or mock) low-level Linux
-- 'sysfs' GPIO operations.
--
-- This type class chiefly exists in order to use a 'sysfs' mock for
-- testing with the 'System.GPIO.Linux.Sysfs.SysfsT'
-- transformer/interpreter (see
-- 'System.GPIO.Linux.Sysfs.Mock.SysfsMockT'). Typically, you will not
-- need to use it directly. If you want to write Linux 'sysfs' GPIO
-- programs that run in 'IO', see the "System.GPIO.Linux.Sysfs.IO"
-- module.
--
-- To see the documentation for this type class's methods, see the
-- canonical implementations of these methods in
-- "System.GPIO.Linux.Sysfs.IO". (The low-level GPIO functions in that
-- module have the same name as the methods in this type class.)
class (Monad m) => MonadSysfs m where
  sysfsIsPresent :: m Bool
  pinIsExported :: Pin -> m Bool
  pinHasDirection :: Pin -> m Bool
  exportPin :: Pin -> m ()
  exportPin' :: Pin -> m ()
  unexportPin :: Pin -> m ()
  readPinDirection :: Pin -> m PinDirection
  writePinDirection :: Pin -> PinDirection -> m ()
  writePinDirectionWithValue :: Pin -> PinValue -> m ()
  readPinValue :: Pin -> m PinValue
  threadWaitReadPinValue :: Pin -> m PinValue
  threadWaitReadPinValue' :: Pin -> Int -> m (Maybe PinValue)
  writePinValue :: Pin -> PinValue -> m ()
  pinHasEdge :: Pin -> m Bool
  readPinEdge :: Pin -> m SysfsEdge
  writePinEdge :: Pin -> SysfsEdge -> m ()
  readPinActiveLow :: Pin -> m Bool
  writePinActiveLow :: Pin -> Bool -> m ()
  availablePins :: m [Pin]

instance (MonadSysfs m) => MonadSysfs (IdentityT m) where
  sysfsIsPresent = lift sysfsIsPresent
  pinIsExported = lift . pinIsExported
  pinHasDirection = lift . pinHasDirection
  pinHasEdge = lift . pinHasEdge
  exportPin = lift . exportPin
  exportPin' = lift . exportPin'
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
  exportPin' = lift . exportPin'
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
  exportPin' = lift . exportPin'
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
  exportPin' = lift . exportPin'
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
  exportPin' = lift . exportPin'
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
  exportPin' = lift . exportPin'
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
  exportPin' = lift . exportPin'
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
  exportPin' = lift . exportPin'
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
  exportPin' = lift . exportPin'
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
  exportPin' = lift . exportPin'
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
  exportPin' = lift . exportPin'
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
  exportPin' = lift . exportPin'
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
