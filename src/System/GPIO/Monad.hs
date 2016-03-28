{-|
Module      : System.GPIO.Monad
Description : A monad for GPIO computations
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

A monadic context for GPIO computations.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Safe #-}

module System.GPIO.Monad
       ( -- * MonadGpio class
         MonadGpio(..)
       , withPin
       ) where

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)

import System.GPIO.Types (Pin, PinDirection, PinReadTrigger, PinValue)

-- | A monad type class for GPIO computations.
--
-- 'h' is an abstract pin handle for operating on opened pins.
class Monad m => MonadGpio h m | m -> h where

  -- | Get a list of available GPIO pins on the system.
  --
  -- This command makes a best-effort attempt to find the available
  -- pins, but some systems may not make the complete list available at
  -- runtime. Therefore, there may be more pins available than are
  -- returned by this function.
  pins :: m [Pin]

  -- | Open a pin for use and return a handle to it.
  openPin :: Pin -> m h

  -- | Close a pin; i.e., indicate to the system that you no longer
  -- intend to use the pin via the given handle.
  closePin :: h -> m ()

  -- | Get the pin's 'PinDirection'.
  --
  -- Note that it is not uncommon for a pin's direction to be
  -- immutable, i.e., to be hard-wired as 'In' or 'Out'. On some
  -- systems (e.g., "System.GPIO.Linux.Sysfs"), the direction of
  -- hard-wired pins is not made available at run-time. In such cases,
  -- this function returns 'Nothing'.
  getPinDirection :: h -> m (Maybe PinDirection)

  -- | Set the pin's 'PinDirection'.
  --
  -- As some pins' direction cannot be changed, you should first call
  -- 'getPinDirection' on the pin handle to make sure this particular
  -- pin's direction is configurable. It is an error to call this
  -- function if the pin's direction cannot be changed.
  setPinDirection :: h -> PinDirection -> m ()

  -- | Toggle the pin's 'PinDirection'.
  --
  -- If the pin's direction cannot be changed, this function returns
  -- 'Nothing'. Otherwise, it returns the new direction.
  togglePinDirection :: h -> m (Maybe PinDirection)

  -- | Sample the pin's /logical/ 'PinValue', where "sample" means
  -- "read the value without blocking." Note that the pin's /physical/
  -- line level is the inverse of the returned value when the pin's
  -- active level is 'Low'. (See 'getPinActiveLevel'.)
  samplePin :: h -> m PinValue

  -- | Read the pin's /logical/ 'PinValue', potentially blocking the
  -- current thread until an event corresponding to the pin's
  -- 'PinReadTrigger' occurs.
  --
  -- If the pin does not support blocking reads, then this function
  -- behaves like 'samplePin' and returns the pin's logical value
  -- without blocking.
  --
  -- Note: due to its interaction with the threading system, this
  -- function may behave differently across different implementations
  -- of Haskell. It has only been tested with GHC.
  readPin :: h -> m PinValue

  -- | Same as 'readPin', except with a timeout, specified in
  -- microseconds. If no event occurs before the timeout expires, this
  -- function returns 'Nothing'; otherwise, it returns the pin's
  -- /logical/ 'PinValue' wrapped in a 'Just'.
  --
  -- If the timeout value is negative, this function behaves just like
  -- 'readPin'.
  --
  -- If the pin does not support blocking reads, then this function
  -- behaves like 'samplePin' and returns the pin's logical value
  -- without blocking.
  --
  -- Note: due to its interaction with the threading system, this
  -- function may behave differently across different implementations
  -- of Haskell. It has only been tested with GHC.
  readPinTimeout :: h -> Int -> m (Maybe PinValue)

  -- | Set the pin's /logical/ 'PinValue'. It is an error to call this
  -- function when the pin is not configured for output.
  writePin :: h -> PinValue -> m ()

  -- | Configure the pin for output and simultaneously set its
  -- 'PinValue'. As long as the pin can be configured for output, you
  -- can call this function regardless of the pin's current
  -- 'PinDirection'. If the pin cannot be configured for output, it is
  -- an error to call this function. (See 'getPinDirection' to
  -- determine safely whether the pin can be configured for output.)
  --
  -- On some platforms (e.g., Linux 'sysfs' GPIO), this operation is
  -- atomic, permitting glitch-free operation when configuring an
  -- output pin's initial value. If the platform can't guarantee
  -- atomic operation, this command is performed as two separate steps
  -- (first setting the direction to 'Out', and then setting the
  -- 'PinValue').
  writePin' :: h -> PinValue -> m ()

  -- | Toggle the pin's 'PinValue'. It is an error to call this
  -- function when the pin is not configured for output.
  togglePinValue :: h -> m PinValue

  -- | Get the pin's 'PinReadTrigger' mode.
  --
  -- Some pins (or GPIO platforms) may not support edge- or
  -- level-triggered blocking reads. In such cases, this function
  -- returns 'Nothing'.
  getPinReadTrigger :: h -> m (Maybe PinReadTrigger)

  -- | Set the pin's 'PinReadTrigger' mode.
  --
  -- Some pins (or entire platforms) may not support edge- or
  -- level-triggered blocking reads. In such cases, it is an error to
  -- call this function. To determine whether a pin supports blocking
  -- reads, call 'getReadPinTrigger' on the pin.
  setPinReadTrigger :: h -> PinReadTrigger -> m ()

  -- | Get the pin's "active" level: 'Low' means the pin is configured
  -- to be active low, and 'High' means the pin is configured to be
  -- active high.
  getPinActiveLevel :: h -> m PinValue

  -- | Set the pin's "active" level, 'Low' or 'High'.
  setPinActiveLevel :: h -> PinValue -> m ()

instance (MonadGpio h m) => MonadGpio h (IdentityT m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

instance (MonadGpio h m) => MonadGpio h (ContT r m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

instance (MonadGpio h m) => MonadGpio h (ExceptT e m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

instance (MonadGpio h m) => MonadGpio h (ListT m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

instance (MonadGpio h m) => MonadGpio h (MaybeT m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

instance (MonadGpio h m) => MonadGpio h (ReaderT r m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

instance (MonadGpio h m, Monoid w) => MonadGpio h (LazyRWS.RWST r w s m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

instance (MonadGpio h m, Monoid w) => MonadGpio h (StrictRWS.RWST r w s m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

instance (MonadGpio h m) => MonadGpio h (LazyState.StateT s m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

instance (MonadGpio h m) => MonadGpio h (StrictState.StateT s m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

instance (MonadGpio h m, Monoid w) => MonadGpio h (LazyWriter.WriterT w m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

instance (MonadGpio h m, Monoid w) => MonadGpio h (StrictWriter.WriterT w m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  samplePin = lift . samplePin
  readPin = lift . samplePin
  readPinTimeout h to = lift $ readPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinReadTrigger = lift . getPinReadTrigger
  setPinReadTrigger h trigger = lift $ setPinReadTrigger h trigger
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v

-- | Exception-safe pin management.
--
-- 'withPin' opens a pin using 'openPin' and passes the handle to
-- the given GPIO computation. Upon completion of the computation or
-- an exception occuring within the computation, 'withPin' closes
-- the handle using 'closePin' and then propagates the result,
-- either by returning the value of the computation or by re-raising
-- the exception. If closing the handle raises an exception, this
-- exception will be raised by 'withPin'.
withPin :: (MonadMask m, MonadGpio h m) => Pin -> (h -> m a) -> m a
withPin p = bracket (openPin p) closePin
