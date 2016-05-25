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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Safe #-}

module System.GPIO.Monad
       ( -- * MonadGpio class
         MonadGpio(..)
       , withPin
         -- * Safer types
         --
         -- | Native GPIO APIs, as a rule, provide more or less the
         -- same interface for all GPIO pins, regardless of their
         -- actual capabilities or configuration. For example, pins
         -- which support interrupts and pins which do not are
         -- typically not represented as a separate types.
         --
         -- One advantage of this approach is that it is quite
         -- flexible. It is, for example, possible to re-configure a
         -- given pin "on the fly" for input, output, interrupts, etc.
         -- However, a drawback of this approach is that it's easy to
         -- make a mistake, e.g., by waiting for interrupts on a pin
         -- that has been configured for output (an operation which,
         -- on Linux, at least, will not raise an error but will block
         -- forever).
         --
         -- The primary goal of the 'MonadGpio' DSL is to make
         -- available to the Haskell programmer as much of the
         -- low-level capabilities of a typical GPIO platform as
         -- possible. As such, it retains both the flexibility of this
         -- one-pin-fits-all approach, and its disadvantages. The
         -- disadvantages are apparent by the number of error
         -- conditions mentioned in the 'MonadGpio' documentation.
         --
         -- However, by trading some of that flexibility for more
         -- restricted types, we can make GPIO programming safer. The
         -- types and actions described below are defined on top of
         -- the 'MonadGpio' DSL and eliminate a whole class of
         -- configuration-related errors.
         --
         -- == A caveat
         --
         -- On some GPIO platforms (e.g., Linux @sysfs@), no provision
         -- is made for opening pins in "exclusive mode," and as such,
         -- pins can be opened and configured by any number of
         -- processes on the system other than our own programs.
         -- Therefore, even when using these safer types, a robust
         -- @gpio@ program should still be prepared to deal with
         -- configuration-related errors in case another process
         -- re-configures a pin underneath our noses.
         --
         -- In other words, even when using these safer types, you
         -- should still be prepared to handle the full range of
         -- 'System.GPIO.Types.SomeGpioException's.
       , InputPin
       , withInputPin
       , readInputPin
       , getInputPinActiveLevel
       , setInputPinActiveLevel
       , toggleInputPinActiveLevel
       , InterruptPin
       , withInterruptPin
       , readInterruptPin
       , pollInterruptPin
       , pollInterruptPinTimeout
       , getInterruptPinInterruptMode
       , setInterruptPinInterruptMode
       , getInterruptPinActiveLevel
       , setInterruptPinActiveLevel
       , toggleInterruptPinActiveLevel
       , OutputPin
       , withOutputPin
       , writeOutputPin
       , toggleOutputPin
       , readOutputPin
       , getOutputPinActiveLevel
       , setOutputPinActiveLevel
       , toggleOutputPinActiveLevel
         -- * Exceptions
       , GpioException(..)
       ) where

import Prelude ()
import Prelude.Compat
import Control.Monad.Catch (Exception(..), MonadMask, MonadThrow, bracket, throwM)
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
import Data.Data

import System.GPIO.Types
       (Pin, PinActiveLevel(..), PinDirection(..), PinInterruptMode(..),
        PinValue(..), gpioExceptionToException, gpioExceptionFromException)

-- | A monad type class for GPIO computations. The type class
-- specifies a DSL for writing portable GPIO programs, and instances
-- of the type class provide the interpreter needed to run these
-- programs on a particular GPIO platform.
--
-- In the type signature, 'h' represents a (platform-dependent)
-- abstract pin handle for operating on opened pins. It is analogous
-- to a file handle.
--
-- == Active-high versus active-low logic
--
-- The DSL supports both /active-high/ and /active-low/ logic. That
-- is, the /active level/ of a GPIO pin can be configured as
-- 'ActiveHigh' or 'ActiveLow'. If a pin's active level is
-- 'ActiveHigh', then for that pin, a 'PinValue' of 'High' corresponds
-- to a "high" physical signal level, and a 'PinValue' of 'Low'
-- corresponds to a "low" physical signal level. The converse is true
-- when the pin's active level is 'ActiveLow'.
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
-- to a "pin value" or "signal level," unless otherwise noted, we mean
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
  -- returned by this action.
  pins :: m [Pin]

  -- | Open a pin for use and return a handle to it.
  --
  -- Note that on some platforms (notably Linux), pin handles are
  -- global resources and it is, strictly speaking, an error to
  -- attempt to open a pin which has already been opened. However,
  -- because there is generally no way to perform an atomic "only open
  -- the pin if it hasn't already been opened" operation on such
  -- platforms, this action will squash that particular error on those
  -- platforms and return the global handle anyway, without making any
  -- other state changes to the already-opened pin.
  --
  -- Keep in mind, however, that on these platforms where pin handles
  -- are global resources, closing one pin handle will effectively
  -- invalidate all other handles for the same pin. Be very careful to
  -- coordinate the opening and closing of pins if you are operating
  -- on the same pin in multiple threads.
  openPin :: Pin -> m h

  -- | Close the pin; i.e., indicate to the system that you no longer
  -- intend to use the pin via the given handle.
  --
  -- Note that on some platforms (notably Linux), pin handles are
  -- global resources and it is, strictly speaking, an error to
  -- attempt to close a pin which has already been closed via another
  -- handle to the same pin. However, this action will squash that
  -- error on those platforms and will simply return without making
  -- any changes to the GPIO environment.
  --
  -- Keep in mind, however, that on these platforms where pin handles
  -- are global resources, opening multiple handles for the same pin
  -- and then closing one of those handles will render all other
  -- handles for the same pin invalid. Be very careful to coordinate
  -- the opening and closing of pins if you are operating on the same
  -- pin in multiple threads.
  --
  -- Note that there are also platforms (again, notably certain Linux
  -- systems) where some pins are effectively always open and cannot
  -- be closed. Invoking this action on such a pin will squash any
  -- error that occurs when attempting to close the pin, and the
  -- action will simply return without making any changes to the GPIO
  -- environment.
  closePin :: h -> m ()

  -- | Get the pin's 'PinDirection'.
  --
  -- Note that it is not uncommon for a pin's direction to be
  -- immutable, i.e., to be hard-wired as 'In' or 'Out'. On some
  -- systems (e.g., "System.GPIO.Linux.Sysfs"), the direction of
  -- hard-wired pins is not made available at run-time. In such cases,
  -- this action returns 'Nothing'.
  getPinDirection :: h -> m (Maybe PinDirection)

  -- | Set the pin's 'PinDirection'.
  --
  -- As some pins' direction cannot be changed, you should first call
  -- 'getPinDirection' on the pin handle to make sure this particular
  -- pin's direction is configurable. It is an error to call this
  -- action if the pin's direction cannot be changed.
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
  -- If the pin's direction cannot be changed, this action returns
  -- 'Nothing'. Otherwise, it returns the new direction.
  togglePinDirection :: h -> m (Maybe PinDirection)

  -- | Read the pin's value.
  --
  -- Note that this action never blocks.
  readPin :: h -> m PinValue

  -- | Block the current thread until an event occurs on the pin which
  -- corresponds to the pin's current interrupt mode. Upon detection
  -- of the event, return the pin's value.
  --
  -- If the pin does not support interrupts, then this action's
  -- behavior is plaform-dependent. To determine whether the pin
  -- supports interrupts, see 'getPinInterruptMode'.
  --
  -- Note: due to its interaction with the threading system, this
  -- action may behave differently across different implementations of
  -- Haskell. It has only been tested with GHC. (On GHC, you should
  -- compile any program that uses this action with the @-threaded@
  -- option.)
  pollPin :: h -> m PinValue

  -- | Same as 'pollPin', except with a timeout, specified in
  -- microseconds. If no event occurs before the timeout expires, this
  -- action returns 'Nothing'; otherwise, it returns the pin's signal
  -- level wrapped in a 'Just'.
  --
  -- If the timeout value is negative, this action behaves just like
  -- 'pollPin'.
  --
  -- If the pin does not support interrupts, then this action's
  -- behavior is platform-dependent. To determine whether the pin
  -- supports interrupts, see 'getPinInterruptMode'.
  --
  -- Note: due to its interaction with the threading system, this
  -- action may behave differently across different implementations of
  -- Haskell. It has only been tested with GHC. (On GHC, you should
  -- compile any program that uses this action with the @-threaded@
  -- option.)
  pollPinTimeout :: h -> Int -> m (Maybe PinValue)

  -- | Set the pin's signal level. It is an error to call this
  -- action when the pin is not configured for output.
  writePin :: h -> PinValue -> m ()

  -- | Configure the pin for output and simultaneously set its signal
  -- level. As long as the pin can be configured for output, you can
  -- call this action regardless of the pin's current
  -- 'PinDirection'. If the pin cannot be configured for output, it is
  -- an error to call this action. (See 'getPinDirection' to
  -- determine safely whether the pin can be configured for output.)
  --
  -- On some platforms (e.g., Linux @sysfs@ GPIO), this operation is
  -- atomic ("glitch-free"), such that the pin will drive the given
  -- value immediately upon being configured for output. If the
  -- platform can't guarantee atomic operation, this command is
  -- performed as two separate steps (first setting the direction to
  -- 'Out', and then setting the 'PinValue'), so the pin may glitch
  -- (i.e., briefly drive the opposite value before the proper value
  -- can be set).
  --
  -- NB: this DSL action is subtly different than its native
  -- equivalent on Linux @sysfs@ GPIO. See
  -- 'System.GPIO.Linux.Sysfs.Monad.writePinDirectionWithValue' for
  -- details.
  writePin' :: h -> PinValue -> m ()

  -- | Toggle the pin's signal level. It is an error to call this
  -- action when the pin is not configured for output.
  --
  -- Returns the pin's new signal level.
  togglePinValue :: h -> m PinValue

  -- | Get the pin's interrupt mode.
  --
  -- Some pins may not support edge- or level-triggered blocking
  -- reads. In such cases, this action returns 'Nothing'.
  --
  -- (Note that 'RisingEdge' and 'FallingEdge' are relative to the
  -- pin's active level; i.e., they refer to the pin's /logical/
  -- signal edges, not its physical signal edges.)
  getPinInterruptMode :: h -> m (Maybe PinInterruptMode)

  -- | Set the pin's interrupt mode (only when the pin is configured
  -- for input).
  --
  -- A pin's interrupt mode determines the behavior of the 'pollPin'
  -- and 'pollPinTimeout' actions. Those actions will block the
  -- current thread on an input pin until a particular event occurs on
  -- that pin's signal waveform: a low-to-high transition
  -- ('RisingEdge'), a high-to-low transition ('FallingEdge'), or any
  -- change of level ('Level').
  --
  -- You can also disable interrupts on the pin so that 'pollPin' will
  -- block the current thread indefinitely (or until a timer expires,
  -- in the case of 'pollPinTimeout'). This functionality is useful
  -- when, for example, one thread is dedicated to servicing
  -- interrupts on a pin, and another thread wants to mask interrupts
  -- on that pin for some period of time.
  --
  -- Some pins (or even some GPIO platforms) may not support
  -- interrupts. In such cases, it is an error to call this action. To
  -- determine whether an pin supports interrupts, call
  -- 'getPinInterruptMode' on the pin.
  --
  -- It is an error to use this action on a pin configured for output.
  setPinInterruptMode :: h -> PinInterruptMode -> m ()

  -- | Get the pin's active level.
  getPinActiveLevel :: h -> m PinActiveLevel

  -- | Set the pin's active level.
  setPinActiveLevel :: h -> PinActiveLevel -> m ()

  -- | Toggle the pin's active level. Returns the pin's new level.
  togglePinActiveLevel :: h -> m PinActiveLevel

instance (MonadGpio h m) => MonadGpio h (IdentityT m) where
  pins = lift pins
  openPin = lift . openPin
  closePin = lift . closePin
  getPinDirection = lift . getPinDirection
  setPinDirection h dir = lift $ setPinDirection h dir
  togglePinDirection = lift . togglePinDirection
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
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
  readPin = lift . readPin
  pollPin = lift . readPin
  pollPinTimeout h to = lift $ pollPinTimeout h to
  writePin h v = lift $ writePin h v
  writePin' h v = lift $ writePin' h v
  togglePinValue = lift . togglePinValue
  getPinInterruptMode = lift . getPinInterruptMode
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
  getPinActiveLevel = lift . getPinActiveLevel
  setPinActiveLevel h v = lift $ setPinActiveLevel h v
  togglePinActiveLevel = lift . togglePinActiveLevel

-- | Exception-safe pin management.
--
-- 'withPin' opens a pin using 'openPin' and passes the handle to the
-- given GPIO computation. Upon completion of the computation, or an
-- exception occuring within the computation, 'withPin' closes the
-- handle using 'closePin' and then propagates the result, either by
-- returning the value of the computation or by re-raising the
-- exception.
withPin :: (MonadMask m, MonadGpio h m) => Pin -> (h -> m a) -> m a
withPin p = bracket (openPin p) closePin

-- | A handle to a pin that's been configured for non-blocking reads
-- only.
--
-- You cannot poll an 'InputPin' for interrupts. See 'InterruptPin'.
newtype InputPin h =
  InputPin {_inputHandle :: h}
  deriving (Eq,Show)

maybeSetPinActiveLevel :: (MonadGpio h m) => h -> Maybe PinActiveLevel -> m ()
maybeSetPinActiveLevel _ Nothing = return ()
maybeSetPinActiveLevel h (Just v) = setPinActiveLevel h v

-- | Like 'withPin', but for 'InputPin's.
--
-- If the optional active level argument is 'Nothing', then the pin's
-- active level is unchanged from its current state. Otherwise, the
-- pin's active level is set to the specified level.
--
-- If the pin cannot be configured for input, this action will error
-- as 'setPinDirection' does.
withInputPin :: (MonadMask m, MonadGpio h m) => Pin -> Maybe PinActiveLevel -> (InputPin h -> m a) -> m a
withInputPin p l action =
  withPin p $ \h ->
    do setPinDirection h In
       maybeSetPinActiveLevel h l
       action $ InputPin h

-- | Like 'readPin'.
readInputPin :: (MonadGpio h m) => InputPin h -> m PinValue
readInputPin p =
  readPin (_inputHandle p)

-- | Like 'getPinActiveLevel'.
getInputPinActiveLevel :: (MonadGpio h m) => InputPin h -> m PinActiveLevel
getInputPinActiveLevel p =
  getPinActiveLevel (_inputHandle p)

-- | Like 'setPinActiveLevel'.
setInputPinActiveLevel :: (MonadGpio h m) => InputPin h -> PinActiveLevel -> m ()
setInputPinActiveLevel p =
  setPinActiveLevel (_inputHandle p)

-- | Like 'togglePinActiveLevel'.
toggleInputPinActiveLevel :: (MonadGpio h m) => InputPin h -> m PinActiveLevel
toggleInputPinActiveLevel p =
  togglePinActiveLevel (_inputHandle p)

-- | A handle to a pin that's been configured both for non-blocking
-- reads and for interrupt-driven polling reads.
newtype InterruptPin h =
  InterruptPin {_interruptHandle :: h}
  deriving (Eq,Show)

-- | Like 'withPin', but for 'InterruptPin's.
--
-- The pin is opened for input and its initial interrupt mode set to
-- the specified mode. (The interrupt mode can be changed after
-- opening, see 'setInterruptPinInterruptMode'.)
--
-- If the optional active level argument is 'Nothing', then the pin's
-- active level is unchanged from its current state. Otherwise, the
-- pin's active level is set to the specified level.
--
-- If the pin cannot be configured for input, this action will error
-- as 'setPinDirection' does.
--
-- If the pin does not support interrupts, this action will error as
-- 'setPinInterruptMode' does.
withInterruptPin :: (MonadMask m, MonadGpio h m) => Pin -> PinInterruptMode -> Maybe PinActiveLevel -> (InterruptPin h -> m a) -> m a
withInterruptPin p mode l action =
  withPin p $ \h ->
    do setPinDirection h In
       setPinInterruptMode h mode
       maybeSetPinActiveLevel h l
       action $ InterruptPin h

-- | Like 'readPin'.
readInterruptPin :: (MonadGpio h m) => InterruptPin h -> m PinValue
readInterruptPin p =
  readPin (_interruptHandle p)

-- | Like 'pollPin'.
pollInterruptPin :: (MonadGpio h m) => InterruptPin h -> m PinValue
pollInterruptPin p =
  pollPin (_interruptHandle p)

-- | Like 'pollPinTimeout'.
pollInterruptPinTimeout :: (MonadGpio h m) => InterruptPin h -> Int -> m (Maybe PinValue)
pollInterruptPinTimeout p =
  pollPinTimeout (_interruptHandle p)

-- | Like 'getPinInterruptMode'.
--
-- Note that, unlike 'getPinInterruptMode', this action does not wrap
-- its result in a 'Maybe', because the only way to get a valid
-- 'InterruptPin' handle is via 'withInterruptPin', and that action
-- will fail in the first place if the pin does not support
-- interrupts.
--
-- (However, in the (theoretically impossible) case that the pin
-- suddenly stops supporting interrupts after the fact, this action
-- will throw a 'GpioException', which explains the 'MonadThrow' @m@
-- constraint.)
getInterruptPinInterruptMode :: (MonadThrow m, MonadGpio h m) => InterruptPin h -> m PinInterruptMode
getInterruptPinInterruptMode p =
  getPinInterruptMode (_interruptHandle p) >>= \case
    Just mode -> return mode
    Nothing -> throwM $ GpioInternalError "The specified InterruptPin does not support interrupts"

-- | Like 'setPinInterruptMode'.
setInterruptPinInterruptMode :: (MonadGpio h m) => InterruptPin h -> PinInterruptMode -> m ()
setInterruptPinInterruptMode p =
  setPinInterruptMode (_interruptHandle p)

-- | Like 'getPinActiveLevel'.
getInterruptPinActiveLevel :: (MonadGpio h m) => InterruptPin h -> m PinActiveLevel
getInterruptPinActiveLevel p =
  getPinActiveLevel (_interruptHandle p)

-- | Like 'setPinActiveLevel'.
setInterruptPinActiveLevel :: (MonadGpio h m) => InterruptPin h -> PinActiveLevel -> m ()
setInterruptPinActiveLevel p =
  setPinActiveLevel (_interruptHandle p)

-- | Like 'togglePinActiveLevel'.
toggleInterruptPinActiveLevel :: (MonadGpio h m) => InterruptPin h -> m PinActiveLevel
toggleInterruptPinActiveLevel p =
  togglePinActiveLevel (_interruptHandle p)

-- | A handle to a pin that's been configured for output only.
--
-- Note that output pins can be both read and written. However, they
-- only support non-blocking reads, not interrupt-driven polling
-- reads.
newtype OutputPin h =
  OutputPin {_outputHandle :: h}
  deriving (Eq,Show)

-- | Like 'withPin', but for 'OutputPin's.
--
-- If the optional active level argument is 'Nothing', then the pin's
-- active level is unchanged from its current state. Otherwise, the
-- pin's active level is set to the specified level.
--
-- The 'PinValue' argument specifies the pin's initial output value.
-- It is relative to the active level argument, or to the pin's
-- current active level if the active level argument is 'Nothing'.
-- Note that if the platform supports glitch-free output, then this
-- action will use that capability to drive the initial output value;
-- see 'writePin'' for details.
--
-- If the pin cannot be configured for output, this action will error
-- as 'setPinDirection' does.
withOutputPin :: (MonadMask m, MonadGpio h m) => Pin -> Maybe PinActiveLevel -> PinValue -> (OutputPin h -> m a) -> m a
withOutputPin p l v action =
  withPin p $ \h ->
    do maybeSetPinActiveLevel h l
       writePin' h v
       action $ OutputPin h

-- | Like 'writePin'.
writeOutputPin :: (MonadGpio h m) => OutputPin h -> PinValue -> m ()
writeOutputPin p =
  writePin (_outputHandle p)

-- | Like 'togglePinValue'.
toggleOutputPin :: (MonadGpio h m) => OutputPin h -> m PinValue
toggleOutputPin p =
  togglePinValue (_outputHandle p)

-- | Like 'readPin'.
readOutputPin :: (MonadGpio h m) => OutputPin h -> m PinValue
readOutputPin p =
  readPin (_outputHandle p)

-- | Like 'getPinActiveLevel'.
getOutputPinActiveLevel :: (MonadGpio h m) => OutputPin h -> m PinActiveLevel
getOutputPinActiveLevel p =
  getPinActiveLevel (_outputHandle p)

-- | Like 'setPinActiveLevel'.
setOutputPinActiveLevel :: (MonadGpio h m) => OutputPin h -> PinActiveLevel -> m ()
setOutputPinActiveLevel p =
  setPinActiveLevel (_outputHandle p)

-- | Like 'togglePinActiveLevel'.
toggleOutputPinActiveLevel :: (MonadGpio h m) => OutputPin h -> m PinActiveLevel
toggleOutputPinActiveLevel p =
  togglePinActiveLevel (_outputHandle p)

-- | Exceptions that are thrown at the 'MonadGpio' DSL layer.
data GpioException
  = GpioInternalError String
    -- ^ An internal error has occurred in the 'MonadGpio' DSL,
    -- something which should "never happen" and should be reported to
    -- the package maintainer.
  deriving (Eq,Show,Typeable)

instance Exception GpioException where
    toException = gpioExceptionToException
    fromException = gpioExceptionFromException
