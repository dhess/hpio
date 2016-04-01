{-|
Module      : System.GPIO.Linux.Sysfs.Monad
Description : Monads for Linux @sysfs@ GPIO operations
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Monad type classes and instances for Linux @sysfs@ GPIO operations.

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module System.GPIO.Linux.Sysfs.Monad
       ( -- * MonadSysfs class
         MonadSysfs(..)
         -- * GPIO via @sysfs@
       , PinDescriptor(..)
       , SysfsGpioT(..)
         -- * Low-level @sysfs@ GPIO functions.
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
       , exportPin'
       , unexportPin
       , pinHasDirection
       , readPinDirection
       , writePinDirection
       , writePinDirectionWithValue
       , readPinValue
       , threadWaitReadPinValue
       , threadWaitReadPinValue'
       , writePinValue
       , pinHasEdge
       , readPinEdge
       , writePinEdge
       , readPinActiveLow
       , writePinActiveLow
       ) where

import Prelude hiding (readFile, writeFile)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, filterM, void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM)
import Control.Monad.Catch.Pure (CatchT)
import Control.Monad.Cont (MonadCont, ContT)
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)
import Control.Monad.Writer (MonadWriter)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8 (readInt, unpack)
import Data.List (isPrefixOf, sort)
import Foreign.C.Types (CInt(..))
import System.FilePath ((</>), takeFileName)

import System.GPIO.Linux.Sysfs.Types (SysfsEdge(..), SysfsException(..), toPinReadTrigger, toSysfsEdge)
import System.GPIO.Linux.Sysfs.Util
import System.GPIO.Monad (MonadGpio(..))
import System.GPIO.Types

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
  -- @sysfs@ can handle this -- but Haskell's writeFile cannot, as it
  -- locks the file and prevents multiple writers. We don't want this
  -- behavior, so we use low-level operations to get around it.
  unlockedWriteFile :: FilePath -> ByteString -> m ()
  -- | Poll a @sysfs@ file for reading, as in POSIX.1-2001 @poll(2)@.
  --
  -- Note that the implementation of this function is only guaranteed
  -- to work for @sysfs@ files, which have a peculiar way of signaling
  -- readiness for reads, and you should not use it for any other
  -- purpose.
  pollFile :: FilePath -> Int -> m CInt

instance (MonadSysfs m) => MonadSysfs (IdentityT m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m) => MonadSysfs (ContT r m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m) => MonadSysfs (CatchT m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m) => MonadSysfs (ExceptT e m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m) => MonadSysfs (ListT m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m) => MonadSysfs (MaybeT m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m) => MonadSysfs (ReaderT r m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m, Monoid w) => MonadSysfs (LazyRWS.RWST r w s m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m, Monoid w) => MonadSysfs (StrictRWS.RWST r w s m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m) => MonadSysfs (LazyState.StateT s m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m) => MonadSysfs (StrictState.StateT s m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m, Monoid w) => MonadSysfs (LazyWriter.WriterT w m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

instance (MonadSysfs m, Monoid w) => MonadSysfs (StrictWriter.WriterT w m) where
  doesDirectoryExist = lift . doesDirectoryExist
  doesFileExist = lift . doesFileExist
  getDirectoryContents = lift . getDirectoryContents
  readFile = lift . readFile
  writeFile fn bs = lift $ writeFile fn bs
  unlockedWriteFile fn bs = lift $ unlockedWriteFile fn bs
  pollFile fn timeout = lift $ pollFile fn timeout

-- | An instance of 'MonadGpio' which translates operations in that
-- monad to operations on Linux's native @sysfs@ GPIO interface.
newtype SysfsGpioT m a =
  SysfsGpioT {runSysfsGpioT :: m a}
  deriving (Functor,Alternative,Applicative,Monad,MonadFix,MonadPlus,MonadThrow,MonadCatch,MonadMask,MonadCont,MonadIO,MonadReader r,MonadError e,MonadWriter w,MonadState s,MonadRWS r w s)

instance MonadTrans SysfsGpioT where
  lift = SysfsGpioT

-- | The @sysfs@ pin handle type. Currently it's just a newtype
-- wrapper around a 'Pin'. The constructor is exported for
-- convenience, but note that the implementation may change in future
-- versions of the package.
newtype PinDescriptor = PinDescriptor { _pin :: Pin } deriving (Show, Eq, Ord)

instance (MonadThrow m, MonadSysfs m) => MonadGpio PinDescriptor (SysfsGpioT m) where
  pins =
    lift sysfsIsPresent >>= \case
      False -> return []
      True -> lift availablePins

  openPin p =
    lift sysfsIsPresent >>= \case
      False -> throwM SysfsNotPresent
      True ->
        do lift $ exportPin' p
           return $ PinDescriptor p

  closePin (PinDescriptor p) = lift $ unexportPin p

  getPinDirection (PinDescriptor p) =
    lift (pinHasDirection p) >>= \case
      False -> return Nothing
      True ->
        do dir <- lift $ readPinDirection p
           return $ Just dir

  setPinDirection (PinDescriptor p) dir =
    lift $ writePinDirection p dir

  togglePinDirection h =
    getPinDirection h >>= \case
      Nothing -> return Nothing
      Just dir ->
        do let newDir = invertDirection dir
           void $ setPinDirection h newDir
           return $ Just newDir

  samplePin (PinDescriptor p) = lift $ readPinValue p

  readPin (PinDescriptor p) = lift $ threadWaitReadPinValue p

  readPinTimeout (PinDescriptor p) timeout =
    lift $ threadWaitReadPinValue' p timeout

  writePin (PinDescriptor p) v =
    lift $ writePinValue p v

  writePin' (PinDescriptor p) v =
    lift $ writePinDirectionWithValue p v

  togglePinValue h =
    do val <- samplePin h
       let newVal = invertValue val
       void $ writePin h newVal
       return newVal

  getPinReadTrigger (PinDescriptor p) =
    lift (pinHasEdge p) >>= \case
      False -> return Nothing
      True ->
        do edge <- lift $ readPinEdge p
           return $ Just (toPinReadTrigger edge)

  setPinReadTrigger (PinDescriptor p) trigger =
    lift $ writePinEdge p $ toSysfsEdge trigger

  -- N.B.: @sysfs@'s @active_low@ attribute is the opposite of
  -- 'MonadGpio''s "active level"!
  getPinActiveLevel (PinDescriptor p) =
    do activeLow <- lift $ readPinActiveLow p
       return $ boolToValue (not activeLow)

  -- N.B.: see 'getPinActiveLevel'.
  setPinActiveLevel (PinDescriptor p) v =
    lift $ writePinActiveLow p $ valueToBool (invertValue v)

-- | Test whether the @sysfs@ GPIO filesystem is available.
sysfsIsPresent :: (MonadSysfs m) => m Bool
sysfsIsPresent = doesDirectoryExist sysfsPath

-- | Test whether the given pin is already exported.
pinIsExported :: (MonadSysfs m) => Pin -> m Bool
pinIsExported = doesDirectoryExist . pinDirName

-- | Export the given pin.
--
-- Note that it's an error to call this function to export a pin
-- that's already been exported.
exportPin :: (MonadSysfs m) => Pin -> m ()
exportPin (Pin n) = unlockedWriteFile exportFileName (intToBS n)

-- | Export the given pin, but, unlike 'exportPin', if the pin is
-- already exported, this is not an error; in this situation, the pin
-- remains exported and its state unchanged.
exportPin' :: (MonadSysfs m) => Pin -> m ()
exportPin' p =
  pinIsExported p >>= \case
    True -> return ()
    False -> exportPin p

-- | Unexport the given pin.
--
-- It is an error to call this function if the pin is not currently
-- exported.
unexportPin :: (MonadSysfs m) => Pin -> m ()
unexportPin (Pin n) = unlockedWriteFile unexportFileName (intToBS n)

-- | Test whether the given pin's direction can be set via the
-- @sysfs@ GPIO filesystem. (Some pins have a hard-wired direction,
-- in which case their direction must be determined by some other
-- mechanism as the @direction@ attribute does not exist for such
-- pins.)
pinHasDirection :: (MonadSysfs m) => Pin -> m Bool
pinHasDirection = doesFileExist . pinDirectionFileName

-- | Read the given pin's direction.
--
-- It is an error to call this function if the pin has no @direction@
-- attribute.
readPinDirection :: (MonadSysfs m, MonadThrow m) => Pin -> m PinDirection
readPinDirection p =
  readFile (pinDirectionFileName p) >>= \case
    "in\n"  -> return In
    "out\n" -> return Out
    x     -> throwM $ UnexpectedDirection p (C8.unpack x)

-- | Set the given pin's direction.
--
-- It is an error to call this function if the pin has no @direction@
-- attribute.
writePinDirection :: (MonadSysfs m) => Pin -> PinDirection -> m ()
writePinDirection p d = writeFile (pinDirectionFileName p) (pinDirectionToBS d)

-- | Pins whose direction can be set may be configured for output by
-- writing a 'PinValue' to their @direction@ attribute. This enables
-- glitch-free output configuration, assuming the pin is currently
-- configured for input, or some kind of tri-stated or floating
-- high-impedance mode.
--
-- It is an error to call this function if the pin has no
-- @direction@ attribute.
writePinDirectionWithValue :: (MonadSysfs m) => Pin -> PinValue -> m ()
writePinDirectionWithValue p v = writeFile (pinDirectionFileName p) (pinDirectionValueToBS v)

-- | Read the given pin's value.
--
-- Note that this function never blocks, regardless of the pin's
-- @edge@ attribute setting.
readPinValue :: (MonadSysfs m, MonadThrow m) => Pin -> m PinValue
readPinValue p =
  readFile (pinValueFileName p) >>= \case
    "0\n" -> return Low
    "1\n" -> return High
    x   -> throwM $ UnexpectedValue p (C8.unpack x)

-- | A blocking version of 'readPinValue'. The current thread will
-- block until an event occurs on the pin as specified by the pin's
-- current @edge@ attribute setting.
--
-- If the pin has no @edge@ attribute, then this function will not
-- block and will act like 'readPinValue'.
threadWaitReadPinValue :: (Functor m, MonadSysfs m, MonadThrow m) => Pin -> m PinValue
threadWaitReadPinValue p =
  threadWaitReadPinValue' p (-1) >>= \case
    Just v -> return v
    -- Yes, I really do mean "error" here. 'Nothing' can only occur
    -- when the poll has timed out, but the (-1) timeout value above
    -- means the poll must either wait forever or fail.
    Nothing -> error "threadWaitReadPinValue timed out, and it should not have. Please file a bug at https://github.com/dhess/gpio"

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
-- If the pin has no @edge@ attribute, then this function will not
-- block and will act like 'readPinValue'.
threadWaitReadPinValue' :: (Functor m, MonadSysfs m, MonadThrow m) => Pin -> Int -> m (Maybe PinValue)
threadWaitReadPinValue' p timeout =
  do pollResult <- pollFile (pinValueFileName p) timeout
     if pollResult > 0
       then Just <$> readPinValue p
       else return Nothing

-- | Set the given pin's value.
--
-- It is an error to call this function if the pin is configured as
-- an input pin.
writePinValue :: (MonadSysfs m) => Pin -> PinValue -> m ()
writePinValue p v = writeFile (pinValueFileName p) (pinValueToBS v)

-- | Test whether the pin has an @edge@ attribute, i.e., whether it
-- can be configured for edge- or level-triggered interrupts.
pinHasEdge :: (MonadSysfs m) => Pin -> m Bool
pinHasEdge p = doesFileExist (pinEdgeFileName p)

-- | Read the given pin's @edge@ attribute.
--
-- It is an error to call this function when the pin has no @edge@
-- attribute.
readPinEdge :: (MonadSysfs m, MonadThrow m) => Pin -> m SysfsEdge
readPinEdge p =
  readFile (pinEdgeFileName p) >>= \case
    "none\n"  -> return None
    "rising\n" -> return Rising
    "falling\n" -> return Falling
    "both\n" -> return Both
    x     -> throwM $ UnexpectedEdge p (C8.unpack x)

-- | Write the given pin's @edge@ attribute.
--
-- It is an error to call this function when the pin has no @edge@
-- attribute.
writePinEdge :: (MonadSysfs m) => Pin -> SysfsEdge -> m ()
writePinEdge p v = writeFile (pinEdgeFileName p) (sysfsEdgeToBS v)

-- | Read the given pin's @active_low@ attribute.
readPinActiveLow :: (MonadSysfs m, MonadThrow m) => Pin -> m Bool
readPinActiveLow p =
  readFile (pinActiveLowFileName p) >>= \case
    "0\n" -> return False
    "1\n" -> return True
    x   -> throwM $ UnexpectedActiveLow p (C8.unpack x)

-- | Write the given pin's @active_low@ attribute.
writePinActiveLow :: (MonadSysfs m) => Pin -> Bool -> m ()
writePinActiveLow p v = writeFile (pinActiveLowFileName p) (activeLowToBS v)

-- | Return a list of all pins that are exposed via the @sysfs@ GPIO
-- filesystem. Note that the returned list may omit some pins that
-- are available on the host but which, for various reasons, are not
-- exposed via the @sysfs@ GPIO filesystem.
availablePins :: (MonadSysfs m, MonadThrow m) => m [Pin]
availablePins =
  do sysfsEntries <- getDirectoryContents sysfsPath
     let sysfsContents = fmap (sysfsPath </>) sysfsEntries
     sysfsDirectories <- filterM doesDirectoryExist sysfsContents
     let chipDirs = filter (isPrefixOf "gpiochip" . takeFileName) sysfsDirectories
     gpioPins <- mapM pinRange chipDirs
     return $ sort $ concat gpioPins

-- Helper functions that aren't exported.
--

readIntFromFile :: (MonadSysfs m, MonadThrow m) => FilePath -> m Int
readIntFromFile f =
  do contents <- readFile f
     case C8.readInt contents of
       Just (n, _) -> return n
       Nothing -> throwM $ UnexpectedContents f (C8.unpack contents)

pinRange :: (MonadSysfs m, MonadThrow m) => FilePath -> m [Pin]
pinRange chipDir =
  do base <- readIntFromFile (chipDir </> "base")
     ngpio <- readIntFromFile (chipDir </> "ngpio")
     if base >= 0 && ngpio > 0
        then return $ fmap Pin [base .. (base + ngpio - 1)]
        else return []
