{-|
Module      : System.GPIO.Linux.Sysfs.Monad
Description : Monads for Linux @sysfs@ GPIO operations
Copyright   : (c) 2018, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <dhess-src@quixoftic.com>
Stability   : experimental
Portability : non-portable

Monad type classes and instances for Linux @sysfs@ GPIO operations.

-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.GPIO.Linux.Sysfs.Monad
       ( -- * MonadSysfs class
         MonadSysfs(..)
         -- * GPIO via @sysfs@
       , PinDescriptor(..)
       , SysfsGpioT(..)
         -- * Convenient constraint synonyms for 'MonadSysfs' signatures.
       , CatchSysfsM
       , ThrowSysfsM
       , ThrowCatchSysfsM
         -- * Low-level @sysfs@ GPIO actions.
         --
         -- If you wish, you can bypass the portable GPIO computation
         -- layer provided by 'MonadGpio' and program directly to the
         -- Linux @sysfs@ GPIO interface in the 'MonadSysfs' monad.
         -- This requires only one level of abstraction (choosing a
         -- 'MonadSysfs' instance) rather than two (both a
         -- 'MonadSysfs' instance /and/ the 'SysfsGpioT' 'MonadGpio'
         -- instance).
       , sysfsIsPresent
       , availablePins
       , pinIsExported
       , exportPin
       , exportPinChecked
       , unexportPin
       , unexportPinChecked
       , pinHasDirection
       , readPinDirection
       , writePinDirection
       , writePinDirectionWithValue
       , readPinValue
       , pollPinValue
       , pollPinValueTimeout
       , writePinValue
       , pinHasEdge
       , readPinEdge
       , writePinEdge
       , readPinActiveLow
       , writePinActiveLow
       ) where

import Protolude hiding (readFile, writeFile)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, catchIOError, throwM)
import Control.Monad.Catch.Pure (CatchT)
import Control.Monad.Cont (MonadCont, ContT)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Logger
       (LoggingT, MonadLogger, MonadLoggerIO, NoLoggingT)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Control
       (ComposeSt, MonadBaseControl(..), MonadTransControl(..),
        defaultLiftBaseWith, defaultRestoreM)
import Control.Monad.Trans.Identity (IdentityT)
import "transformers" Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)
import Control.Monad.Writer (MonadWriter)
import qualified Data.ByteString.Char8 as C8 (readInt)
import qualified Data.Set as Set (empty, fromList)
import Foreign.C.Types (CInt(..))
import qualified GHC.IO.Exception as IO (IOErrorType(..))
import System.FilePath ((</>), takeFileName)
import System.IO.Error
       (IOError, ioeGetErrorType, isAlreadyInUseError,
        isDoesNotExistError, isPermissionError)

import System.GPIO.Linux.Sysfs.Types (SysfsEdge(..), SysfsException(..), toPinInterruptMode, toSysfsEdge)
import System.GPIO.Linux.Sysfs.Util
       (intToBS, pinActiveLowFileName, pinDirectionFileName,
        pinEdgeFileName, pinValueFileName, pinDirName, activeLowToBS,
        pinDirectionToBS, pinDirectionValueToBS, sysfsEdgeToBS,
        pinValueToBS, sysfsPath, exportFileName, unexportFileName)
import System.GPIO.Monad (MonadGpio(..), withPin)
import System.GPIO.Types
       (Pin(..), PinActiveLevel(..), PinCapabilities(..),
        PinDirection(..), PinInputMode(..), PinOutputMode(..),
        PinValue(..), invertValue)

-- | A type class for monads which implement (or mock) low-level Linux
-- @sysfs@ GPIO operations.
class (Monad m) => MonadSysfs m where

  -- | Equivalent to 'System.Directory.doesDirectoryExist'.
  doesDirectoryExist :: FilePath -> m Bool

  -- | Equivalent to 'System.Directory.doesFileExist'.
  doesFileExist :: FilePath -> m Bool

  -- | Equivalent to 'System.Directory.getDirectoryContents'.
  getDirectoryContents :: FilePath -> m [FilePath]

  -- | Equivalent to 'Data.ByteString.readFile'.
  readFile :: FilePath -> m ByteString

  -- | Equivalent to 'Data.ByteString.writeFile'.
  writeFile :: FilePath -> ByteString -> m ()

  -- | @sysfs@ control files which are global shared resources may be
  -- written simultaneously by multiple threads. This is fine --
  -- @sysfs@ can handle this -- but Haskell's
  -- 'Data.ByteString.writeFile' cannot, as it locks the file and
  -- prevents multiple writers. We don't want this behavior, so we use
  -- low-level operations to get around it.
  unlockedWriteFile :: FilePath -> ByteString -> m ()

  -- | Poll a @sysfs@ file for reading, as in POSIX.1-2001 @poll(2)@.
  --
  -- Note that the implementation of this action is only guaranteed to
  -- work for @sysfs@ files, which have a peculiar way of signaling
  -- readiness for reads. Do not use it for any other purpose.
  pollFile :: FilePath -> Int -> m CInt

  default doesDirectoryExist :: (MonadTrans t, MonadSysfs m', t m' ~ m) =>
    FilePath -> m Bool
  default doesFileExist :: (MonadTrans t, MonadSysfs m', t m' ~ m) =>
    FilePath -> m Bool
  default getDirectoryContents :: (MonadTrans t, MonadSysfs m', t m' ~ m) =>
    FilePath -> m [FilePath]
  default readFile :: (MonadTrans t, MonadSysfs m', t m' ~ m) =>
    FilePath -> m ByteString
  default writeFile :: (MonadTrans t, MonadSysfs m', t m' ~ m) =>
    FilePath -> ByteString -> m ()
  default unlockedWriteFile :: (MonadTrans t, MonadSysfs m', t m' ~ m) =>
    FilePath -> ByteString -> m ()
  default pollFile :: (MonadTrans t, MonadSysfs m', t m' ~ m) =>
    FilePath -> Int -> m CInt

  doesDirectoryExist = lift . doesDirectoryExist
  {-# INLINE doesDirectoryExist #-}
  doesFileExist = lift . doesFileExist
  {-# INLINE doesFileExist #-}
  getDirectoryContents = lift . getDirectoryContents
  {-# INLINE getDirectoryContents #-}
  readFile = lift . readFile
  {-# INLINE readFile #-}
  writeFile fn bs = lift $ writeFile fn bs
  {-# INLINE writeFile #-}
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  {-# INLINE unlockedWriteFile #-}
  pollFile fn timeout = lift $ pollFile fn timeout
  {-# INLINE pollFile #-}

instance (MonadSysfs m) => MonadSysfs (IdentityT m)
instance (MonadSysfs m) => MonadSysfs (ContT r m)
instance (MonadSysfs m) => MonadSysfs (CatchT m)
instance (MonadSysfs m) => MonadSysfs (ExceptT e m)
instance (MonadSysfs m) => MonadSysfs (ListT m)
instance (MonadSysfs m) => MonadSysfs (MaybeT m)
instance (MonadSysfs m) => MonadSysfs (ReaderT r m)
instance (MonadSysfs m, Monoid w) => MonadSysfs (LazyRWS.RWST r w s m)
instance (MonadSysfs m, Monoid w) => MonadSysfs (StrictRWS.RWST r w s m)
instance (MonadSysfs m) => MonadSysfs (LazyState.StateT s m)
instance (MonadSysfs m) => MonadSysfs (StrictState.StateT s m)
instance (MonadSysfs m, Monoid w) => MonadSysfs (LazyWriter.WriterT w m)
instance (MonadSysfs m, Monoid w) => MonadSysfs (StrictWriter.WriterT w m)
instance (MonadSysfs m) => MonadSysfs (LoggingT m)
instance (MonadSysfs m) => MonadSysfs (NoLoggingT m)

-- | The @sysfs@ pin handle type. Currently it's just a newtype
-- wrapper around a 'Pin'. The constructor is exported for
-- convenience, but note that the implementation may change in future
-- versions of the package.
newtype PinDescriptor =
  PinDescriptor {_pin :: Pin}
  deriving (Show,Eq,Ord)

-- | An instance of 'MonadGpio' which translates actions in that monad
-- to operations on Linux's native @sysfs@ GPIO interface.
newtype SysfsGpioT m a = SysfsGpioT
  { runSysfsGpioT :: m a
  } deriving ( Functor
             , Alternative
             , Applicative
             , Monad
             , MonadBase b
             , MonadFix
             , MonadPlus
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadCont
             , MonadIO
             , MonadReader r
             , MonadError e
             , MonadWriter w
             , MonadState s
             , MonadRWS r w s
             , MonadLogger
             , MonadLoggerIO
             )

instance MonadTrans SysfsGpioT where
  lift = SysfsGpioT

instance MonadBaseControl b m => MonadBaseControl b (SysfsGpioT m) where
  type StM (SysfsGpioT m) a = ComposeSt SysfsGpioT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadTransControl SysfsGpioT where
  type StT SysfsGpioT a = a
  liftWith f = SysfsGpioT $ f runSysfsGpioT
  restoreT = SysfsGpioT
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

type CatchSysfsM m = (Functor m, MonadCatch m, MonadSysfs m)
type ThrowSysfsM m = (Functor m, MonadThrow m, MonadSysfs m)
type ThrowCatchSysfsM m = (Functor m, MonadThrow m, MonadCatch m, MonadSysfs m)

instance (MonadMask m, ThrowCatchSysfsM m) => MonadGpio PinDescriptor (SysfsGpioT m) where
  pins =
    lift sysfsIsPresent >>= \case
      False -> return []
      True -> lift availablePins

  -- The @sysfs@ GPIO interface is particularly information-poor. It
  -- is not currently possible, in a hardware-independent way, to
  -- determine which particular input and output modes a pin supports,
  -- for example.
  --
  -- For input pins, therefore, we can only claim 'InputDefault'
  -- support. However, for output pins, it's possible to emulate both
  -- 'OutputOpenDrain' and 'OutputOpenSource' modes by switching the
  -- pin into input mode for 'High' (in the case of 'OutputOpenDrain')
  -- or 'Low' ('OutputOpenSource') values. We do not currently support
  -- this, but it's a planned feature.
  --
  -- If a pin has no @direction@ attribute, it means there is no
  -- hardware-independent way to determine its hard-wired direction
  -- via @sysfs@. That means there's no practical way to use it with
  -- the cross-platform DSL, so in this case we simply report the pin
  -- as having no capabilities.
  pinCapabilities p =
    lift sysfsIsPresent >>= \case
      False -> throwM SysfsNotPresent
      True ->
        withPin p $ \_ ->
          do hasDir <- lift $ pinHasDirection p
             hasEdge <- lift $ pinHasEdge p
             if hasDir
                then return $ PinCapabilities (Set.fromList [InputDefault])
                                              (Set.fromList [OutputDefault])
                                              hasEdge
                else return $ PinCapabilities Set.empty Set.empty False

  openPin p =
    lift sysfsIsPresent >>= \case
      False -> throwM SysfsNotPresent
      True ->
        do lift $ exportPin p
           return $ PinDescriptor p

  closePin (PinDescriptor p) = lift $ unexportPin p

  getPinDirection (PinDescriptor p) =
    lift $ readPinDirection p

  getPinInputMode (PinDescriptor p) =
    do dir <- lift $ readPinDirection p
       if dir == In
          then return InputDefault
          else throwM $ InvalidOperation p

  setPinInputMode (PinDescriptor p) mode =
    if mode == InputDefault
       then lift $ writePinDirection p In
       else throwM $ UnsupportedInputMode mode p

  getPinOutputMode (PinDescriptor p) =
    do dir <- lift $ readPinDirection p
       if dir == Out
          then return OutputDefault
          else throwM $ InvalidOperation p

  setPinOutputMode (PinDescriptor p) mode v =
    if mode == OutputDefault
       then lift $ writePinDirectionWithValue p v
       else throwM $ UnsupportedOutputMode mode p

  readPin (PinDescriptor p) = lift $ readPinValue p

  pollPin (PinDescriptor p) = lift $ pollPinValue p

  pollPinTimeout (PinDescriptor p) timeout =
    lift $ pollPinValueTimeout p timeout

  writePin (PinDescriptor p) v =
    lift $ writePinValue p v

  togglePin h =
    do val <- readPin h
       let newVal = invertValue val
       void $ writePin h newVal
       return newVal

  getPinInterruptMode (PinDescriptor p) =
    do edge <- lift $ readPinEdge p
       return $ toPinInterruptMode edge

  setPinInterruptMode (PinDescriptor p) mode =
    lift $ writePinEdge p $ toSysfsEdge mode

  getPinActiveLevel (PinDescriptor p) =
    do activeLow <- lift $ readPinActiveLow p
       return $ activeLowToActiveLevel activeLow

  setPinActiveLevel (PinDescriptor p) l =
    lift $ writePinActiveLow p $ activeLevelToActiveLow l

  togglePinActiveLevel (PinDescriptor p) =
    do toggled <- not <$> lift (readPinActiveLow p)
       lift $ writePinActiveLow p toggled
       return $ activeLowToActiveLevel toggled

activeLevelToActiveLow :: PinActiveLevel -> Bool
activeLevelToActiveLow ActiveLow = True
activeLevelToActiveLow ActiveHigh = False

activeLowToActiveLevel :: Bool -> PinActiveLevel
activeLowToActiveLevel False = ActiveHigh
activeLowToActiveLevel True = ActiveLow

-- | Test whether the @sysfs@ GPIO filesystem is available.
sysfsIsPresent :: (MonadSysfs m) => m Bool
sysfsIsPresent = doesDirectoryExist sysfsPath

-- | Test whether the pin is already exported.
pinIsExported :: (MonadSysfs m) => Pin -> m Bool
pinIsExported = doesDirectoryExist . pinDirName

-- | Export the given pin.
--
-- Note that, if the pin is already exported, this is not an error; in
-- this situation, the pin remains exported and its state unchanged.
exportPin :: (CatchSysfsM m) => Pin -> m ()
exportPin pin@(Pin n) =
  catchIOError
    (unlockedWriteFile exportFileName (intToBS n))
    mapIOError
  where
    mapIOError :: (MonadThrow m) => IOError -> m ()
    mapIOError e
      | isAlreadyInUseError e = return ()
      | isInvalidArgumentError e = throwM $ InvalidPin pin
      | isPermissionError e = throwM $ PermissionDenied pin
      | otherwise = throwM e

-- | Export the given pin.
--
-- Note that, unlike 'exportPin', it's an error to call this action to
-- export a pin that's already been exported. This is the standard
-- Linux @sysfs@ GPIO behavior.
exportPinChecked :: (CatchSysfsM m) => Pin -> m ()
exportPinChecked pin@(Pin n) =
  catchIOError
    (unlockedWriteFile exportFileName (intToBS n))
    mapIOError
  where
    mapIOError :: (MonadThrow m) => IOError -> m ()
    mapIOError e
      | isAlreadyInUseError e = throwM $ AlreadyExported pin
      | isInvalidArgumentError e = throwM $ InvalidPin pin
      | isPermissionError e = throwM $ PermissionDenied pin
      | otherwise = throwM e

-- | Unexport the given pin.
--
-- Note that, if the pin is already unexported or cannot be
-- unexported, this is not an error. In this situation, the pin
-- remains exported and its state unchanged.
unexportPin :: (CatchSysfsM m) => Pin -> m ()
unexportPin pin@(Pin n) =
  catchIOError
    (unlockedWriteFile unexportFileName (intToBS n))
    mapIOError
  where
    mapIOError :: (MonadThrow m) => IOError -> m ()
    mapIOError e
      | isInvalidArgumentError e = return ()
      | isPermissionError e = throwM $ PermissionDenied pin
      | otherwise = throwM e

-- | Unexport the given pin.
--
-- Note that, unlike 'unexportPin', it is an error to call this action
-- if the pin is not currently exported. This is the standard Linux
-- @sysfs@ GPIO behavior.
unexportPinChecked :: (CatchSysfsM m) => Pin -> m ()
unexportPinChecked pin@(Pin n) =
  catchIOError
    (unlockedWriteFile unexportFileName (intToBS n))
    mapIOError
  where
    mapIOError :: (MonadThrow m) => IOError -> m ()
    mapIOError e
      | isInvalidArgumentError e = throwM $ NotExported pin
      | isPermissionError e = throwM $ PermissionDenied pin
      | otherwise = throwM e

-- | Test whether the pin's direction can be set via the @sysfs@ GPIO
-- filesystem. (Some pins have a hard-wired direction, in which case
-- their direction must be determined by some other mechanism, as the
-- @direction@ attribute does not exist for such pins.)
pinHasDirection :: (ThrowSysfsM m) => Pin -> m Bool
pinHasDirection p =
  do exported <- pinIsExported p
     if exported
        then doesFileExist (pinDirectionFileName p)
        else throwM $ NotExported p

-- | Read the pin's direction.
--
-- It is an error to call this action if the pin has no @direction@
-- attribute.
readPinDirection :: (ThrowCatchSysfsM m) => Pin -> m PinDirection
readPinDirection p =
  catchIOError
    (readFile (pinDirectionFileName p) >>= \case
       "in\n"  -> return In
       "out\n" -> return Out
       x     -> throwM $ UnexpectedDirection p (decodeUtf8 x))
    mapIOError
  where
    mapIOError :: (ThrowSysfsM m) => IOError -> m PinDirection
    mapIOError e
      | isDoesNotExistError e =
          do exported <- pinIsExported p
             if exported
                then throwM $ NoDirectionAttribute p
                else throwM $ NotExported p
      | isPermissionError e = throwM $ PermissionDenied p
      | otherwise = throwM e

-- | Set the pin's direction.
--
-- It is an error to call this action if the pin has no @direction@
-- attribute.
--
-- Note that, in Linux @sysfs@ GPIO, changing a pin's direction to
-- @out@ will also set its /physical/ signal level to @low@.
--
-- NB: in Linux @sysfs@, if an input pin is cofigured for edge- or
-- level-triggered reads, it's an error to set its direction to @out@.
-- However, this action will handle that case gracefully by setting
-- the pin's @edge@ attribute to @none@ before setting the pin's
-- direction to @out@.
writePinDirection :: (CatchSysfsM m) => Pin -> PinDirection -> m ()
writePinDirection p In =
  writeDirection p (pinDirectionToBS In)
writePinDirection p Out =
  do resetEdge p
     writeDirection p (pinDirectionToBS Out)

-- | Pins whose direction can be set may be configured for output by
-- writing a 'PinValue' to their @direction@ attribute, such that the
-- given value will be driven on the pin as soon as it's configured
-- for output. This enables glitch-free output configuration, assuming
-- the pin is currently configured for input, or some kind of
-- tri-stated or floating high-impedance mode.
--
-- It is an error to call this action if the pin has no @direction@
-- attribute.
--
-- NB: for some unfathomable reason, writing @high@ or @low@ to a
-- pin's @direction@ attribute sets its /physical/ signal level; i.e.,
-- it ignores the value of the pin's @active_low@ attribute. Contrast
-- this behavior with the behavior of writing to the pin's @value@
-- attribute, which respects the value of the pin's @active_low@
-- attribute and sets the pin's /logical/ signal level.
--
-- Rather than slavishly following the Linux @sysfs@ GPIO spec, we
-- choose to be consistent by taking into account the pin's active
-- level when writing the @direction@ attribute. In other words, the
-- 'PinValue' argument to this action is the /logical/ signal level
-- that will be set on the pin. If you're using this action to program
-- directly to the Linux @sysfs@ GPIO interface and expecting things
-- to behave as they do with raw @sysfs@ GPIO operations, keep this in
-- mind!
writePinDirectionWithValue :: (CatchSysfsM m) => Pin -> PinValue -> m ()
writePinDirectionWithValue p v =
  do activeLow <- readPinActiveLow p
     let f = if activeLow then invertValue else identity
     resetEdge p
     writeDirection p (pinDirectionValueToBS $ f v)

resetEdge :: (CatchSysfsM m) => Pin -> m ()
resetEdge p =
  maybeReadPinEdge >>= \case
    Nothing -> return ()
    Just None -> return ()
    _ -> writePinEdge p None
  where
    maybeReadPinEdge :: (CatchSysfsM m) => m (Maybe SysfsEdge)
    maybeReadPinEdge =
        pinHasEdge p >>= \case
          False -> return Nothing
          True -> Just <$> readPinEdge p

writeDirection :: (CatchSysfsM m) => Pin -> ByteString -> m ()
writeDirection p bs =
  catchIOError
    (writeFile (pinDirectionFileName p) bs)
    mapIOError
  where
    mapIOError :: (ThrowSysfsM m) => IOError -> m ()
    mapIOError e
      | isDoesNotExistError e =
          do exported <- pinIsExported p
             if exported
                then throwM $ NoDirectionAttribute p
                else throwM $ NotExported p
      | isPermissionError e = throwM $ PermissionDenied p
      | otherwise = throwM e

-- | Read the pin's signal level.
--
-- Note that this action never blocks, regardless of the pin's @edge@
-- attribute setting.
readPinValue :: (ThrowCatchSysfsM m) => Pin -> m PinValue
readPinValue p =
  catchIOError
    (readFile (pinValueFileName p) >>= \case
       "0\n" -> return Low
       "1\n" -> return High
       x   -> throwM $ UnexpectedValue p (decodeUtf8 x))
    mapIOError
  where
    mapIOError :: (ThrowSysfsM m) => IOError -> m PinValue
    mapIOError e
      | isDoesNotExistError e = throwM $ NotExported p
      | isPermissionError e = throwM $ PermissionDenied p
      | otherwise = throwM e

-- | A blocking version of 'readPinValue'. The current thread will
-- block until an event occurs on the pin as specified by the pin's
-- current @edge@ attribute setting.
--
-- If the pin has no @edge@ attribute, then this action's behavior is
-- undefined. (Most likely, it will block indefinitely.)
pollPinValue :: (ThrowCatchSysfsM m) => Pin -> m PinValue
pollPinValue p =
  pollPinValueTimeout p (-1) >>= \case
     Just v -> return v
     -- 'Nothing' can only occur when the poll has timed out, but the
     -- (-1) timeout value above means the poll must either wait
     -- forever or fail; so this indicates a major problem.
     Nothing -> throwM $
       InternalError "pollPinValue timed out, and it should not have. Please file a bug at https://github.com/quixoftic/gpio"

-- | Same as 'pollPinValue', except that a timeout value,
-- specified in microseconds, is provided. If no event occurs before
-- the timeout expires, this action returns 'Nothing'; otherwise, it
-- returns the pin's value wrapped in a 'Just'.
--
-- If the timeout value is negative, this action behaves just like
-- 'pollPinValue'.
--
-- When specifying a timeout value, be careful not to exceed
-- 'maxBound'.
--
-- If the pin has no @edge@ attribute, then this action's behavior is
-- undefined. (Most likely, it will time out after the specified delay
-- and return 'Nothing'.)
--
-- NB: the curent implementation of this action limits the timeout
-- precision to 1 millisecond, rather than 1 microsecond as the
-- timeout parameter implies.
pollPinValueTimeout :: (ThrowCatchSysfsM m) => Pin -> Int -> m (Maybe PinValue)
pollPinValueTimeout p timeout =
  catchIOError
    (do pollResult <- pollFile (pinValueFileName p) timeout
        if pollResult > 0
          then Just <$> readPinValue p
          else return Nothing)
    mapIOError
  where
    mapIOError :: (ThrowSysfsM m) => IOError -> m (Maybe PinValue)
    mapIOError e
      | isDoesNotExistError e = throwM $ NotExported p
      | isPermissionError e = throwM $ PermissionDenied p
      | otherwise = throwM e

-- | Set the pin's signal level.
--
-- It is an error to call this action if the pin is configured as an
-- input pin.
writePinValue :: (CatchSysfsM m) => Pin -> PinValue -> m ()
writePinValue p v =
  catchIOError
    (writeFile (pinValueFileName p) (pinValueToBS v))
    mapIOError
  where
    mapIOError :: (ThrowSysfsM m) => IOError -> m ()
    mapIOError e
      | isDoesNotExistError e = throwM $ NotExported p
      | isPermissionError e = throwM $ PermissionDenied p
      | otherwise = throwM e

-- | Test whether the pin has an @edge@ attribute, i.e., whether it
-- can be configured for edge- or level-triggered interrupts.
pinHasEdge :: (ThrowSysfsM m) => Pin -> m Bool
pinHasEdge p =
  do exported <- pinIsExported p
     if exported
        then doesFileExist (pinEdgeFileName p)
        else throwM $ NotExported p

-- | Read the pin's @edge@ attribute.
--
-- It is an error to call this action when the pin has no @edge@
-- attribute.
readPinEdge :: (ThrowCatchSysfsM m) => Pin -> m SysfsEdge
readPinEdge p =
  catchIOError
    (readFile (pinEdgeFileName p) >>= \case
       "none\n"  -> return None
       "rising\n" -> return Rising
       "falling\n" -> return Falling
       "both\n" -> return Both
       x     -> throwM $ UnexpectedEdge p (decodeUtf8 x))
    mapIOError
  where
    mapIOError :: (ThrowSysfsM m) => IOError -> m SysfsEdge
    mapIOError e
      | isDoesNotExistError e =
          do exported <- pinIsExported p
             if exported
                then throwM $ NoEdgeAttribute p
                else throwM $ NotExported p
      | isPermissionError e = throwM $ PermissionDenied p
      | otherwise = throwM e

-- | Write the pin's @edge@ attribute.
--
-- It is an error to call this action when the pin has no @edge@
-- attribute, or when the pin is configured for output.
writePinEdge :: (CatchSysfsM m) => Pin -> SysfsEdge -> m ()
writePinEdge p v =
  catchIOError
    (writeFile (pinEdgeFileName p) (sysfsEdgeToBS v))
    mapIOError
  where
    mapIOError :: (ThrowSysfsM m) => IOError -> m ()
    mapIOError e
      | isDoesNotExistError e =
          do exported <- pinIsExported p
             if exported
                then throwM $ NoEdgeAttribute p
                else throwM $ NotExported p
      | isInvalidArgumentError e = throwM $ InvalidOperation p
      | isPermissionError e = throwM $ PermissionDenied p
      | otherwise = throwM e

-- | Read the pin's @active_low@ attribute.
readPinActiveLow :: (ThrowCatchSysfsM m) => Pin -> m Bool
readPinActiveLow p =
  catchIOError
    (readFile (pinActiveLowFileName p) >>= \case
       "0\n" -> return False
       "1\n" -> return True
       x   -> throwM $ UnexpectedActiveLow p (decodeUtf8 x))
    mapIOError
  where
    mapIOError :: (ThrowSysfsM m) => IOError -> m Bool
    mapIOError e
      | isDoesNotExistError e = throwM $ NotExported p
      | isPermissionError e = throwM $ PermissionDenied p
      | otherwise = throwM e

-- | Write the pin's @active_low@ attribute.
writePinActiveLow :: (CatchSysfsM m) => Pin -> Bool -> m ()
writePinActiveLow p v =
  catchIOError
    (writeFile (pinActiveLowFileName p) (activeLowToBS v))
    mapIOError
  where
    mapIOError :: (ThrowSysfsM m) => IOError -> m ()
    mapIOError e
      | isDoesNotExistError e = throwM $ NotExported p
      | isPermissionError e = throwM $ PermissionDenied p
      | otherwise = throwM e

-- | Return a list of all pins that are exposed via the @sysfs@ GPIO
-- filesystem. Note that the returned list may omit some pins that
-- are available on the host but which, for various reasons, are not
-- exposed via the @sysfs@ GPIO filesystem.
availablePins :: (ThrowCatchSysfsM m) => m [Pin]
availablePins =
  catchIOError
    (do sysfsEntries <- getDirectoryContents sysfsPath
        let sysfsContents = fmap (sysfsPath </>) sysfsEntries
        sysfsDirectories <- filterM doesDirectoryExist sysfsContents
        let chipDirs = filter (isPrefixOf "gpiochip" . takeFileName) sysfsDirectories
        gpioPins <- mapM pinRange chipDirs
        return $ sort $ concat gpioPins)
    mapIOError
  where
    mapIOError :: (ThrowSysfsM m) => IOError -> m [Pin]
    mapIOError e
      | isDoesNotExistError e = throwM SysfsError
      | isPermissionError e = throwM SysfsPermissionDenied
      | otherwise = throwM e

-- Helper actions that aren't exported.
--

readIntFromFile :: (ThrowSysfsM m) => FilePath -> m Int
readIntFromFile f =
  do contents <- readFile f
     case C8.readInt contents of
       Just (n, _) -> return n
       Nothing -> throwM $ UnexpectedContents f (decodeUtf8 contents)

pinRange :: (ThrowSysfsM m) => FilePath -> m [Pin]
pinRange chipDir =
  do base <- readIntFromFile (chipDir </> "base")
     ngpio <- readIntFromFile (chipDir </> "ngpio")
     if base >= 0 && ngpio > 0
        then return $ fmap Pin [base .. (base + ngpio - 1)]
        else return []

-- IOErrorType predicates for the extended GHC.IO.Exception types
-- which we use.

isInvalidArgumentErrorType :: IO.IOErrorType -> Bool
isInvalidArgumentErrorType IO.InvalidArgument = True
isInvalidArgumentErrorType _ = False

isInvalidArgumentError :: IOError -> Bool
isInvalidArgumentError = isInvalidArgumentErrorType . ioeGetErrorType
