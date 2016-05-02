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

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Safe #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module System.GPIO.Monad
       ( -- * MonadGpio class
         MonadGpio(..)
       , withPin
       ) where

#if ! MIN_VERSION_base(4,8,0)
import Prelude.Compat (Monoid)
#endif

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Catch.Pure (CatchT)
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

-- | A monad type class for GPIO computations. The type class
-- specifies a DSL for writing portable GPIO programs, and instances
-- of the type class provide the implementation needed to interpret
-- these programs on a particular GPIO platform.
--
-- In the type signature, 'h' represents a (platform-dependent)
-- abstract pin handle, for operating on opened pins. It is analogous
-- to a file handle.
--
-- == Active-high versus active-low logic
--
-- The DSL supports both active-high and active-low logic. That is,
-- the "active level" of a GPIO pin can be configured as 'High' or
-- 'Low'. If a pin's active level is 'High', then for that pin, a
-- 'PinValue' of 'High' corresponds to a "high" physical signal level,
-- and a 'PinValue' of 'Low' corresponds to a "low" physical signal
-- level. The converse is true when the pin's active level is 'Low'.
--
-- (Note that this explanation oversimplifies the reality of circuit
-- design, as in order to predict the actual voltage level seen on a
-- given pin, one must also consider factors such as
-- open-drain/open-collector output, what other active sources and
-- passive elements are connected to the pin, etc. However, these
-- additional factors are outside the scope of this DSL and must be
-- considered in the context of your particular GPIO hardware and the
-- circuit to which it's connected.)
--
-- Despite the potential confusion, the advantage of supporting
-- active-low logic is that you can, if you choose, write your program
-- in terms of "positive" logic (where 'High' always means "on" and
-- 'Low' always means "off"), and, with the same program, interface
-- with either positive (active-high) or negative (active-low) logic
-- simply by setting the pin's active level before running the
-- program.
--
-- In the documentation for this package, whenever you see a reference
-- to a "pin value" or "signal level", unless otherwise noted, we mean
-- the /logical/ value or level, not the /physical/ value or level;
-- that is, we mean the abstract notion of the pin being "on" or
-- "off," independent of the voltage level seen on the physical pin.
-- If the pin is configured as active-high, then the logical and
-- physical values are one and the same; if not, they are the inverse
-- of each other.
--
-- Note that the active-high/active-low setting is per-pin; each pin's
-- active level is independent of the others.
--
-- Not all platforms natively support active-low logic. On platforms
-- without native support, the platform interpreter will invert values
-- (both read and written) in software when a pin is configured as
-- active-low.

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
  --
  -- Note that, on some GPIO platforms (e.g., Linux @sysfs@), setting
  -- a pin's direction to 'Out' also sets its value to a constant
  -- value; i.e., the platform has no memory of the pin's previous
  -- output value, if any. This value is platform-dependent and
  -- therefore, from the perspective of the DSL, is undefined. If you
  -- want to guarantee that a particular value is driven when a pin is
  -- configured for output, see the 'writePin'' action.
  setPinDirection :: h -> PinDirection -> m ()

  -- | Toggle the pin's 'PinDirection'.
  --
  -- If the pin's direction cannot be changed, this function returns
  -- 'Nothing'. Otherwise, it returns the new direction.
  togglePinDirection :: h -> m (Maybe PinDirection)

  -- | Sample the pin's signal level, where "sample" means "read the
  -- value without blocking."
  samplePin :: h -> m PinValue

  -- | Read the pin's signal level, potentially blocking the current
  -- thread until an event corresponding to the pin's 'PinReadTrigger'
  -- occurs.
  --
  -- If the pin does not support blocking reads, then this function
  -- behaves like 'samplePin' and returns the pin's value without
  -- blocking.
  --
  -- Note: due to its interaction with the threading system, this
  -- function may behave differently across different implementations
  -- of Haskell. It has only been tested with GHC.
  readPin :: h -> m PinValue

  -- | Same as 'readPin', except with a timeout, specified in
  -- microseconds. If no event occurs before the timeout expires, this
  -- function returns 'Nothing'; otherwise, it returns the pin's
  -- signal level wrapped in a 'Just'.
  --
  -- If the timeout value is negative, this function behaves just like
  -- 'readPin'.
  --
  -- If the pin does not support blocking reads, then this function
  -- behaves like 'samplePin' and returns the pin's value without
  -- blocking.
  --
  -- Note: due to its interaction with the threading system, this
  -- function may behave differently across different implementations
  -- of Haskell. It has only been tested with GHC.
  readPinTimeout :: h -> Int -> m (Maybe PinValue)

  -- | Set the pin's signal level. It is an error to call this
  -- function when the pin is not configured for output.
  writePin :: h -> PinValue -> m ()

  -- | Configure the pin for output and simultaneously set its signal
  -- level. As long as the pin can be configured for output, you can
  -- call this function regardless of the pin's current
  -- 'PinDirection'. If the pin cannot be configured for output, it is
  -- an error to call this function. (See 'getPinDirection' to
  -- determine safely whether the pin can be configured for output.)
  --
  -- On some platforms (e.g., Linux 'sysfs' GPIO), this operation is
  -- atomic, such that the pin will drive the given value immediately
  -- upon being configured for output ("glitch-free"). If the platform
  -- can't guarantee atomic operation, this command is performed as
  -- two separate steps (first setting the direction to 'Out', and
  -- then setting the 'PinValue'), so the pin may glitch (i.e.,
  -- briefly drive the opposite value before the proper value can be
  -- set).
  --
  -- NB: this DSL action is subtly different than its native
  -- equivalent on Linux. See
  -- 'System.GPIO.Linux.Sysfs.Monad.writePinDirectionWithValue' for
  -- details.
  writePin' :: h -> PinValue -> m ()

  -- | Toggle the pin's signal level. It is an error to call this
  -- function when the pin is not configured for output.
  --
  -- Returns the pin's new signal level.
  togglePinValue :: h -> m PinValue

  -- | Get the pin's 'PinReadTrigger' mode.
  --
  -- Think of a 'PinReadTrigger' as an interrupt mode. Combined with
  -- the 'readPin' and 'readPinTimeout' actions, you can block the
  -- current thread on a GPIO pin until a particular event occurs: in
  -- logic terms, a leading signal edge ('RisingEdge'), a trailing
  -- signal edge ('FallingEdge'), or any change of level ('Level').
  --
  -- You can also disable interrupts on the pin so that reads will
  -- block indefinitely (or until they time out, in the case of
  -- 'readPinTimeout') until the trigger is set to a different value
  -- again. This functionality is useful when, for example, one thread
  -- is dedicated to servicing interrupts on a pin, and another thread
  -- wants to mask interrupts on that pin for some period of time.
  --
  -- Some pins (or GPIO platforms) may not support edge- or
  -- level-triggered blocking reads. In such cases, this function
  -- returns 'Nothing'; calls to 'readPin' will block indefinitely;
  -- and calls to 'readPinTimeout' will always time out.
  --
  -- (Note that 'RisingEdge' and 'FallingEdge' are relative to the
  -- pin's active level; i.e., they refer to the pin's /logical/
  -- signal edges, not its physical signal edges.)
  getPinReadTrigger :: h -> m (Maybe PinReadTrigger)

  -- | Set the pin's 'PinReadTrigger' mode.
  --
  -- Some pins (or entire platforms) may not support edge- or
  -- level-triggered blocking reads. In such cases, it is an error to
  -- call this function. To determine whether a pin supports blocking
  -- reads, call 'getReadPinTrigger' on the pin.
  setPinReadTrigger :: h -> PinReadTrigger -> m ()

  -- | Get the pin's active level: 'Low' means the pin is configured
  -- for active-low logic, and 'High' means the pin is configured as
  -- active-high.
  getPinActiveLevel :: h -> m PinValue

  -- | Set the pin's active level, 'Low' or 'High'.
  setPinActiveLevel :: h -> PinValue -> m ()

  -- | Toggle the pin's active level. Returns the pin's new level.
  togglePinActiveLevel :: h -> m PinValue

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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
  togglePinActiveLevel = lift . togglePinActiveLevel

instance (MonadGpio h m) => MonadGpio h (CatchT m) where
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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
  togglePinActiveLevel = lift . togglePinActiveLevel

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
