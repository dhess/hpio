{-|
Module      : System.GPIO.Monad
Description : A monad for GPIO computations
Copyright   : (c) 2017, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <dhess-src@quixoftic.com>
Stability   : experimental
Portability : non-portable

A monadic context for GPIO computations.

-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.GPIO.Monad
       ( -- * GPIO types
         --
         -- | For your convenience, the following types are
         -- re-exported from the "System.GPIO.Types" module.
         Pin(..)
       , pinNumber
       , PinInputMode(..)
       , PinOutputMode(..)
       , PinCapabilities(..)
       , PinDirection(..)
       , PinActiveLevel(..)
       , PinValue(..)
       , PinInterruptMode(..)

         -- * Some convenient constraint synonyms for 'MonadGpio' signatures.
       , MaskGpioM
       , ThrowGpioM

         -- * MonadGpio class
       , MonadGpio(..)
       , withPin

         -- * Safer types
         --
         -- | If you can restrict your use of a particular pin to just
         -- one mode of operation (input, interrupt-driven input, or
         -- output), you can achieve better type-safety than is
         -- possible with the fully-general 'Pin' type by using the
         -- one of the following more limited types and its
         -- corresponding actions.
         --
         -- == A caveat
         --
         -- On some GPIO platforms (e.g., Linux @sysfs@), no provision
         -- is made for opening pins in "exclusive mode," and as such,
         -- pins can be opened and configured by any number of
         -- processes on the system other than our own programs.
         -- Therefore, even when using these safer types, a robust
         -- @hpio@ program should still be prepared to deal with
         -- configuration-related errors in case another process
         -- re-configures a pin while the @hpio@ program is using it.
         --
         -- In other words, even when using these safer types, you
         -- should still be prepared to handle the full range of
         -- 'System.GPIO.Types.SomeGpioException's.
       , InputPin
       , withInputPin
       , readInputPin
       , getInputPinInputMode
       , getInputPinActiveLevel
       , setInputPinActiveLevel
       , toggleInputPinActiveLevel
       , InterruptPin
       , withInterruptPin
       , readInterruptPin
       , pollInterruptPin
       , pollInterruptPinTimeout
       , getInterruptPinInputMode
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
       , getOutputPinOutputMode
       , getOutputPinActiveLevel
       , setOutputPinActiveLevel
       , toggleOutputPinActiveLevel

         -- * The GPIO exception hierarchy
         --
         -- | Re-exported from "System.GPIO.Types".
       , SomeGpioException(..)
       , gpioExceptionToException
       , gpioExceptionFromException
       ) where

import Prelude ()
import Prelude.Compat
import Control.Monad.Catch (MonadMask, MonadThrow, bracket)
import Control.Monad.Catch.Pure (CatchT)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Identity (IdentityT)
import "transformers" Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)

import System.GPIO.Types
       (Pin(..), PinInputMode(..), PinOutputMode(..), PinCapabilities(..),
        PinActiveLevel(..), PinDirection(..), PinInterruptMode(..),
        PinValue(..), SomeGpioException(..), gpioExceptionToException,
        gpioExceptionFromException, pinNumber)

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

  -- | Query the pin's capabilities.
  pinCapabilities :: Pin -> m PinCapabilities

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

  -- | Get the pin's currently configured direction.
  --
  -- Note that there is no @setPinDirection@ action. You set the pin's
  -- direction indirectly by setting its input mode or output mode via
  -- 'setPinInputMode' and 'setPinOutputMode', respectively.
  --
  -- Rarely, a particular pin's direction may not be available in a
  -- cross-platform way. In these cases, calling this action is an
  -- error. In general, though, if the pin's capabilities indicate
  -- that it supports at least one 'PinInputMode' or 'PinOutputMode',
  -- it's safe to call this action.
  getPinDirection :: h -> m PinDirection

  -- | Get the pin's input mode.
  --
  -- If the pin is not currently configured for input, it's an error
  -- to call this action.
  getPinInputMode :: h -> m PinInputMode

  -- | Set the pin's input mode. This action will also set the pin's
  -- direction to 'In'.
  --
  -- It is an error to call this action if the given pin does not
  -- support the given input mode.
  setPinInputMode :: h -> PinInputMode -> m ()

  -- | Get the pin's output mode.
  --
  -- If the pin is not currently configured for output, it's an error
  -- to call this action.
  getPinOutputMode :: h -> m PinOutputMode

  -- | Set the pin's output mode and value. This action will also set
  -- the pin's direction to 'Out'
  --
  -- If the pin is already in output mode and you only want to change
  -- its value, use 'writePin'.
  --
  -- It is an error to call this action if the given pin does not
  -- support the given output mode.
  setPinOutputMode :: h -> PinOutputMode -> PinValue -> m ()

  -- | Read the pin's value.
  --
  -- Note that this action never blocks.
  readPin :: h -> m PinValue

  -- | Block the current thread until an event occurs on the pin which
  -- corresponds to the pin's current interrupt mode. Upon detection
  -- of the event, return the pin's value.
  --
  -- If the pin does not support interrupts, then this action's
  -- behavior is plaform-dependent.
  --
  -- It is an error to call this action when the pin is not configured
  -- for input.
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
  -- behavior is platform-dependent.
  --
  -- It is an error to call this action when the pin is not configured
  -- for input.
  --
  -- Note: due to its interaction with the threading system, this
  -- action may behave differently across different implementations of
  -- Haskell. It has only been tested with GHC. (On GHC, you should
  -- compile any program that uses this action with the @-threaded@
  -- option.)
  pollPinTimeout :: h -> Int -> m (Maybe PinValue)

  -- | Set the pin's output value.
  --
  -- It is an error to call this action when the pin is not configured
  -- for output.
  writePin :: h -> PinValue -> m ()

  -- | Toggle the pin's output value and return the pin's new output
  -- value.
  --
  -- It is an error to call this action when the pin is not configured
  -- for output.
  togglePin :: h -> m PinValue

  -- | Get the pin's interrupt mode.
  --
  -- If the pin does not support interrupts, it is an error to call
  -- this action.
  --
  -- (Note that 'RisingEdge' and 'FallingEdge' are relative to the
  -- pin's active level; i.e., they refer to the pin's /logical/
  -- signal edges, not its physical signal edges.)
  getPinInterruptMode :: h -> m PinInterruptMode

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
  -- interrupts. In such cases, it is an error to call this action.
  --
  -- It is an error to use this action on a pin configured for output.
  setPinInterruptMode :: h -> PinInterruptMode -> m ()

  -- | Get the pin's active level.
  getPinActiveLevel :: h -> m PinActiveLevel

  -- | Set the pin's active level.
  setPinActiveLevel :: h -> PinActiveLevel -> m ()

  -- | Toggle the pin's active level. Returns the pin's new level.
  togglePinActiveLevel :: h -> m PinActiveLevel

  default pins :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    m [Pin]
  default pinCapabilities :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    Pin -> m PinCapabilities
  default openPin :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    Pin -> m h
  default closePin :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> m ()
  default getPinDirection :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> m PinDirection
  default getPinInputMode :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> m PinInputMode
  default setPinInputMode :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> PinInputMode -> m ()
  default getPinOutputMode :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> m PinOutputMode
  default setPinOutputMode :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> PinOutputMode -> PinValue -> m ()
  default readPin :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> m PinValue
  default pollPin :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> m PinValue
  default pollPinTimeout :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> Int -> m (Maybe PinValue)
  default writePin :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> PinValue -> m ()
  default togglePin :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> m PinValue
  default getPinInterruptMode :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> m PinInterruptMode
  default setPinInterruptMode :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> PinInterruptMode -> m ()
  default getPinActiveLevel :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> m PinActiveLevel
  default setPinActiveLevel :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> PinActiveLevel -> m ()
  default togglePinActiveLevel :: (MonadTrans t, MonadGpio h' m', t m' ~ m, h' ~ h) =>
    h -> m PinActiveLevel

  pins = lift pins
  {-# INLINE pins #-}
  pinCapabilities = lift . pinCapabilities
  {-# INLINE pinCapabilities #-}
  openPin = lift . openPin
  {-# INLINE openPin #-}
  closePin = lift . closePin
  {-# INLINE closePin #-}
  getPinDirection = lift . getPinDirection
  {-# INLINE getPinDirection #-}
  getPinInputMode = lift . getPinInputMode
  {-# INLINE getPinInputMode #-}
  setPinInputMode h mode = lift $ setPinInputMode h mode
  {-# INLINE setPinInputMode #-}
  getPinOutputMode = lift . getPinOutputMode
  {-# INLINE getPinOutputMode #-}
  setPinOutputMode h mode v = lift $ setPinOutputMode h mode v
  {-# INLINE setPinOutputMode #-}
  readPin = lift . readPin
  {-# INLINE readPin #-}
  pollPin = lift . readPin
  {-# INLINE pollPin #-}
  pollPinTimeout h to = lift $ pollPinTimeout h to
  {-# INLINE pollPinTimeout #-}
  writePin h v = lift $ writePin h v
  {-# INLINE writePin #-}
  togglePin = lift . togglePin
  {-# INLINE togglePin #-}
  getPinInterruptMode = lift . getPinInterruptMode
  {-# INLINE getPinInterruptMode #-}
  setPinInterruptMode h mode = lift $ setPinInterruptMode h mode
  {-# INLINE setPinInterruptMode #-}
  getPinActiveLevel = lift . getPinActiveLevel
  {-# INLINE getPinActiveLevel #-}
  setPinActiveLevel h v = lift $ setPinActiveLevel h v
  {-# INLINE setPinActiveLevel #-}
  togglePinActiveLevel = lift . togglePinActiveLevel
  {-# INLINE togglePinActiveLevel #-}

instance (MonadGpio h m) => MonadGpio h (IdentityT m)
instance (MonadGpio h m) => MonadGpio h (ContT r m)
instance (MonadGpio h m) => MonadGpio h (CatchT m)
instance (MonadGpio h m) => MonadGpio h (ExceptT e m)
instance (MonadGpio h m) => MonadGpio h (ListT m)
instance (MonadGpio h m) => MonadGpio h (MaybeT m)
instance (MonadGpio h m) => MonadGpio h (ReaderT r m)
instance (MonadGpio h m, Monoid w) => MonadGpio h (LazyRWS.RWST r w s m)
instance (MonadGpio h m, Monoid w) => MonadGpio h (StrictRWS.RWST r w s m)
instance (MonadGpio h m) => MonadGpio h (LazyState.StateT s m)
instance (MonadGpio h m) => MonadGpio h (StrictState.StateT s m)
instance (MonadGpio h m, Monoid w) => MonadGpio h (LazyWriter.WriterT w m)
instance (MonadGpio h m, Monoid w) => MonadGpio h (StrictWriter.WriterT w m)

type MaskGpioM h m = (MonadMask m, MonadGpio h m)
type ThrowGpioM h m = (MonadThrow m, MonadGpio h m)

-- | Exception-safe pin management.
--
-- 'withPin' opens a pin using 'openPin' and passes the handle to the
-- given GPIO computation. Upon completion of the computation, or an
-- exception occuring within the computation, 'withPin' closes the
-- handle using 'closePin' and then propagates the result, either by
-- returning the value of the computation or by re-raising the
-- exception.
withPin :: (MaskGpioM h m) => Pin -> (h -> m a) -> m a
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

-- | Like 'withPin', but for 'InputPin's. Sets the pin's input mode to
-- the specified 'PinInputMode' value.
--
-- If the optional active level argument is 'Nothing', then the pin's
-- active level is unchanged from its current state. Otherwise, the
-- pin's active level is set to the specified level.
--
-- It is an error to call this action if the pin cannot be configured
-- for input, or if it does not support the specified input mode.
withInputPin :: (MaskGpioM h m) => Pin -> PinInputMode -> Maybe PinActiveLevel -> (InputPin h -> m a) -> m a
withInputPin p mode l action =
  withPin p $ \h ->
    do setPinInputMode h mode
       maybeSetPinActiveLevel h l
       action $ InputPin h

-- | Like 'readPin'.
readInputPin :: (MonadGpio h m) => InputPin h -> m PinValue
readInputPin p =
  readPin (_inputHandle p)

-- | Like 'getPinInputMode'.
getInputPinInputMode :: (MonadGpio h m) => InputPin h -> m PinInputMode
getInputPinInputMode p =
  getPinInputMode (_inputHandle p)

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

-- | Like 'withPin', but for 'InterruptPin's. The pin is opened for
-- input, is input mode is set to the specified 'PinInputMode' value,
-- and its interrupt mode is set to the specified 'PinInterruptMode'
-- value.
--
-- If the optional active level argument is 'Nothing', then the pin's
-- active level is unchanged from its current state. Otherwise, the
-- pin's active level is set to the specified level.
--
-- It is an error to call this action if any of the following are true:
--
-- * The pin cannot be configured for input.
--
-- * The pin does not support the specified input mode.
--
-- * The pin does not support interrupts.
withInterruptPin :: (MaskGpioM h m) => Pin -> PinInputMode -> PinInterruptMode -> Maybe PinActiveLevel -> (InterruptPin h -> m a) -> m a
withInterruptPin p inputMode interruptMode l action =
  withPin p $ \h ->
    do setPinInputMode h inputMode
       setPinInterruptMode h interruptMode
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

-- | Like 'getPinInputMode'.
getInterruptPinInputMode :: (MonadGpio h m) => InterruptPin h -> m PinInputMode
getInterruptPinInputMode p =
  getPinInputMode (_interruptHandle p)

-- | Like 'getPinInterruptMode'.
getInterruptPinInterruptMode :: (ThrowGpioM h m) => InterruptPin h -> m PinInterruptMode
getInterruptPinInterruptMode p =
  getPinInterruptMode (_interruptHandle p)

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

-- | Like 'withPin', but for 'OutputPin's. Sets the pin's output mode
-- to the specified 'PinOutputMode' value.
--
-- The 'PinValue' argument specifies the pin's initial output value.
-- It is relative to the active level argument, or to the pin's
-- current active level if the active level argument is 'Nothing'.
--
-- It is an error to call this action if the pin cannot be configured
-- for output, or if it does not support the specified output mode.
withOutputPin :: (MaskGpioM h m) => Pin -> PinOutputMode -> Maybe PinActiveLevel -> PinValue -> (OutputPin h -> m a) -> m a
withOutputPin p mode l v action =
  withPin p $ \h ->
    do maybeSetPinActiveLevel h l
       setPinOutputMode h mode v
       action $ OutputPin h

-- | Like 'writePin'.
writeOutputPin :: (MonadGpio h m) => OutputPin h -> PinValue -> m ()
writeOutputPin p =
  writePin (_outputHandle p)

-- | Like 'togglePin'.
toggleOutputPin :: (MonadGpio h m) => OutputPin h -> m PinValue
toggleOutputPin p =
  togglePin (_outputHandle p)

-- | Like 'readPin'.
readOutputPin :: (MonadGpio h m) => OutputPin h -> m PinValue
readOutputPin p =
  readPin (_outputHandle p)

-- | Like 'getPinOutputMode'.
getOutputPinOutputMode :: (MonadGpio h m) => OutputPin h -> m PinOutputMode
getOutputPinOutputMode p =
  getPinOutputMode (_outputHandle p)

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
