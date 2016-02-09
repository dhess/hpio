{-|
Module      : System.GPIO.Linux.Sysfs.IO
Description : The actual Linux 'sysfs' GPIO implementation
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

The actual Linux 'sysfs' GPIO implementation. This implementation will
only function properly on Linux systems with a 'sysfs' GPIO subsystem,
obviously.

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module System.GPIO.Linux.Sysfs.IO
         ( -- * SysfsIOT transformer
           SysfsIOT(..)
         , SysfsIO
         , runSysfsIO
           -- * Low-level 'sysfs' GPIO functions.
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

import Control.Applicative ((<$>), Applicative)
import Control.Monad (filterM, void)
import Control.Monad.Catch
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile, writeFile)
import Data.ByteString.Builder (toLazyByteString, intDec)
import qualified Data.ByteString.Char8 as C8 (readInt, unpack)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import Data.List (isPrefixOf, sort)
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.C.Types (CInt(..))
import qualified Language.C.Inline as C (include)
import qualified Language.C.Inline.Interruptible as CI
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName)
import qualified System.GPIO.Linux.Sysfs.Monad as M
import System.GPIO.Linux.Sysfs.Free
import System.GPIO.Linux.Sysfs.Types
import System.GPIO.Linux.Sysfs.Util
import System.GPIO.Types
import "unix" System.Posix.IO (OpenMode(ReadOnly, WriteOnly), closeFd, defaultFileFlags, openFd)
import "unix-bytestring" System.Posix.IO.ByteString (fdWrite)
import System.Posix.Types (Fd)

-- Our poll(2) wrapper.
--
-- Note: standard practice for Haskell timeouts/delays is to use
-- microseconds; however, poll(2) takes a __milli__second timeout.
-- 'pollSysfs' takes a microsecond timeout argument, but the
-- implementation converts it to milliseconds (in C code) before
-- passing it to poll(2).

-- Because it may block, we use the 'interruptible' variant of the C
-- FFI. This means we need to check for EINTR and try again when it
-- occurs.
--
-- We assume that 'Language.C.Inline.Interruptible' preserves errno
-- when returning from the FFI call!

C.include "<errno.h>"
C.include "<poll.h>"
C.include "<stdint.h>"
C.include "<unistd.h>"

-- Using poll(2) to wait for GPIO interrupts is a bit flaky:
--
-- On certain combinations of kernels+hardware, a "dummy read(2)" is
-- needed before the poll(2) operation. As read(2) on a GPIO sysfs
-- pin's "value" file doesn't block, it doesn't hurt to do this in all
-- cases, anyway.
--
-- The Linux man page for poll(2) states that setting POLLERR in the
-- 'events' field is meaningless. However, the kernel GPIO
-- documentation states: "If you use poll(2), set the events POLLPRI
-- and POLLERR." Here we do what the kernel documentation says.
--
-- When poll(2) returns, an lseek(2) is needed before read(2), per the
-- Linux kernel documentation.
--
-- It appears that poll(2) on the GPIO sysfs pin's "value" file always
-- returns POLLERR in 'revents', even if there is no error. (This is
-- supposedly true for all sysfs files, not just for GPIO.) We simply
-- ignore that bit and only consider the return value of poll(2) to
-- determine whether an error has occurred. (Presumably, if POLLERR is
-- set and poll(2) returns no error, then the subsequent lseek(2) or
-- read(2) will fail.)
--
-- Ref:
-- https://e2e.ti.com/support/dsp/davinci_digital_media_processors/f/716/t/182883
-- http://www.spinics.net/lists/linux-gpio/msg03848.html
-- https://www.kernel.org/doc/Documentation/gpio/sysfs.txt
-- http://stackoverflow.com/questions/16442935/why-doesnt-this-call-to-poll-block-correctly-on-a-sysfs-device-attribute-file
 -- http://stackoverflow.com/questions/27411013/poll-returns-both-pollpri-pollerr
pollSysfs :: Fd -> Int -> IO CInt
pollSysfs fd timeout =
  let cfd = fromIntegral fd
      ctimeout = fromIntegral timeout
  in
    [CI.block| int {
         uint8_t dummy;
         if (read($(int cfd), &dummy, 1) == -1) {
             return -1;
         }

         struct pollfd fds = { .fd = $(int cfd), .events = POLLPRI|POLLERR, .revents = 0 };

         int timeout_in_ms = ($(int ctimeout) > 0) ? ($(int ctimeout) / 1000) : $(int ctimeout);

         int poll_result = poll(&fds, 1, timeout_in_ms);
         if (poll_result == -1)  {
             return -1;
         }
         if (lseek(fds.fd, 0, SEEK_SET) == -1) {
             return -1;
         }
         return poll_result;
     } |]

-- | A monad transformer which adds Linux 'sysfs' GPIO computations to
-- an inner monad 'm'.
--
-- == Interactions with threads
-- Some parts of this GPIO implementation use the Haskell C FFI, and
-- may block on C I/O operations. (Specifically,
-- 'System.GPIO.Free.readPin' will block in the C FFI until its event
-- is triggered.) When using this implementation with GHC, you should
-- compile your program with the @-threaded@ option, so that threads
-- performing these blocking operations do not block other Haskell
-- threads in the system.
--
-- Note that the C FFI bits in this implementation are marked as
-- 'interruptible', so that, on versions of GHC later than 7.8.1,
-- functions such as 'Control.Concurent.throwTo' will work properly
-- when targeting a Haskell thread which is using this implementation.
--
-- (On Haskell implementations other than GHC, the threading
-- implications are unknown; see the implementation's notes on how its
-- threading system interacts with the C FFI.)
newtype SysfsIOT m a =
  SysfsIOT { runSysfsIOT :: m a }
  deriving (Applicative,Functor,Monad,MonadFix,MonadIO,MonadThrow,MonadCatch,MonadMask)

-- | A convenient specialization of 'SysfsT' which runs GPIO eDSL
-- computations in 'IO'.
type SysfsIO a = SysfsT (SysfsIOT IO) (SysfsIOT IO) a

-- | Run a 'SysfsIO' computation in the 'IO' monad and return the
-- result.
runSysfsIO :: SysfsIO a -> IO a
runSysfsIO action = runSysfsIOT $ runSysfsT action

instance MonadTrans SysfsIOT where
  lift = SysfsIOT

instance (MonadIO m, MonadThrow m) => M.MonadSysfs (SysfsIOT m) where
  sysfsIsPresent = liftIO sysfsIsPresent
  pinIsExported = liftIO . pinIsExported
  pinHasDirection = liftIO . pinHasDirection
  pinHasEdge = liftIO . pinHasEdge
  exportPin = liftIO . exportPin
  exportPin' = liftIO . exportPin'
  unexportPin = liftIO . unexportPin
  readPinDirection = liftIO . readPinDirection
  writePinDirection p d = liftIO $ writePinDirection p d
  writePinDirectionWithValue p v = liftIO $ writePinDirectionWithValue p v
  readPinValue = liftIO . readPinValue
  threadWaitReadPinValue = liftIO . threadWaitReadPinValue
  threadWaitReadPinValue' p to = liftIO $ threadWaitReadPinValue' p to
  writePinValue p v = liftIO $ writePinValue p v
  readPinEdge = liftIO . readPinEdge
  writePinEdge p e = liftIO $ writePinEdge p e
  readPinActiveLow = liftIO . readPinActiveLow
  writePinActiveLow p v = liftIO $ writePinActiveLow p v
  availablePins = liftIO availablePins

-- | Test whether the 'sysfs' GPIO filesystem is available.
sysfsIsPresent :: IO Bool
sysfsIsPresent = doesDirectoryExist sysfsPath

-- | Test whether the given pin is already exported.
pinIsExported :: Pin -> IO Bool
pinIsExported p = doesDirectoryExist (pinDirName p)

-- | Test whether the given pin's direction can be set via the
-- 'sysfs' GPIO filesystem. (Some pins have a hard-wired direction,
-- in which case their direction must be determined by some other
-- mechanism as the @direction@ attribute does not exist for such
-- pins.)
pinHasDirection :: Pin -> IO Bool
pinHasDirection p = doesFileExist (pinDirectionFileName p)

-- | Test whether the pin has an @edge@ attribute, i.e., whether it
-- can be configured for edge- or level-triggered interrupts.
pinHasEdge :: Pin -> IO Bool
pinHasEdge p = doesFileExist (pinEdgeFileName p)

-- @sysfs@ control files which are global shared resources may be
-- written simultaneously by multiple threads. This is fine -- @sysfs@
-- can handle this -- but Haskell's writeFile cannot, as it locks the
-- file and prevents multiple writers. We don't want this behavior, so
-- we use low-level operations to get around it.
llWriteFile :: FilePath -> ByteString -> IO ()
llWriteFile fn bs =
  bracket
    (openFd fn WriteOnly Nothing defaultFileFlags)
    closeFd
    (\fd -> void $ fdWrite fd bs)

-- | Export the given pin.
--
-- Note that it's an error to call this function to export a pin
-- that's already been exported.
exportPin :: Pin -> IO ()
exportPin (Pin n) = llWriteFile exportFileName (intToBS n)

-- | Export the given pin, but, unlike 'exportPin', if the pin is
-- already exported, this is not an error; in this situation, the pin
-- remains exported and its state unchanged.
exportPin' :: Pin -> IO ()
exportPin' p =
  pinIsExported p >>= \case
    True -> return ()
    False -> exportPin p

-- | Unexport the given pin.
--
-- It is an error to call this function if the pin is not currently
-- exported.
unexportPin :: Pin -> IO ()
unexportPin (Pin n) = llWriteFile unexportFileName (intToBS n)

-- | Read the given pin's direction.
--
-- It is an error to call this function if the pin has no @direction@
-- attribute.
readPinDirection :: Pin -> IO PinDirection
readPinDirection p =
  BS.readFile (pinDirectionFileName p) >>= \case
    "in\n"  -> return In
    "out\n" -> return Out
    x     -> throwM $ UnexpectedDirection p (C8.unpack x)

-- | Set the given pin's direction.
--
-- It is an error to call this function if the pin has no @direction@
-- attribute.
writePinDirection :: Pin -> PinDirection -> IO ()
writePinDirection p d = BS.writeFile (pinDirectionFileName p) (toSysfsPinDirection d)

-- | Pins whose direction can be set may be configured for output by
-- writing a 'PinValue' to their @direction@ attribute. This enables
-- glitch-free output configuration, assuming the pin is currently
-- configured for input, or some kind of tri-stated or floating
-- high-impedance mode.
--
-- It is an error to call this function if the pin has no
-- @direction@ attribute.
writePinDirectionWithValue :: Pin -> PinValue -> IO ()
writePinDirectionWithValue p v = BS.writeFile (pinDirectionFileName p) (toSysfsPinValue v)

-- | Read the given pin's value.
--
-- Note that this function never blocks, regardless of the pin's
-- @edge@ attribute setting.
readPinValue :: Pin -> IO PinValue
readPinValue p =
  BS.readFile (pinValueFileName p) >>= \case
    "0\n" -> return Low
    "1\n" -> return High
    x   -> throwM $ UnexpectedValue p (C8.unpack x)

-- | A blocking version of 'readPinValue'. The current thread will
-- block until an event occurs on the pin as specified by the pin's
-- current @edge@ attribute setting.
--
-- If the pin has no @edge@ attribute, then this function will not
-- block and will act like 'readPinValue'.
threadWaitReadPinValue :: Pin -> IO PinValue
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
threadWaitReadPinValue' :: Pin -> Int -> IO (Maybe PinValue)
threadWaitReadPinValue' p timeout =
  do pollResult <- bracket
                     (openFd (pinValueFileName p) ReadOnly Nothing defaultFileFlags)
                     closeFd
                     (\fd -> throwErrnoIfMinus1Retry "pollSysfs" $ pollSysfs fd timeout)
     -- Could use fdRead here and handle EAGAIN, but it's easier to
     -- close the fd and use nice handle-based IO, instead. If this
     -- becomes a performance problem... we probably need a
     -- non-sysfs-based interpreter in that case, anyway.
     if pollResult > 0
       then Just <$> readPinValue p
       else return Nothing

-- | Set the given pin's value.
--
-- It is an error to call this function if the pin is configured as
-- an input pin.
writePinValue :: Pin -> PinValue -> IO ()
writePinValue p v = BS.writeFile (pinValueFileName p) (toSysfsPinValue v)

-- | Read the given pin's @edge@ attribute.
--
-- It is an error to call this function when the pin has no @edge@
-- attribute.
readPinEdge :: Pin -> IO SysfsEdge
readPinEdge p =
  BS.readFile (pinEdgeFileName p) >>= \case
    "none\n"  -> return None
    "rising\n" -> return Rising
    "falling\n" -> return Falling
    "both\n" -> return Both
    x     -> throwM $ UnexpectedEdge p (C8.unpack x)

-- | Write the given pin's @edge@ attribute.
--
-- It is an error to call this function when the pin has no @edge@
-- attribute.
writePinEdge :: Pin -> SysfsEdge -> IO ()
writePinEdge p v = BS.writeFile (pinEdgeFileName p) (toSysfsPinEdge v)

-- | Read the given pin's @active_low@ attribute.
readPinActiveLow :: Pin -> IO Bool
readPinActiveLow p =
  BS.readFile (pinActiveLowFileName p) >>= \case
    "0\n" -> return False
    "1\n" -> return True
    x   -> throwM $ UnexpectedActiveLow p (C8.unpack x)

-- | Write the given pin's @active_low@ attribute.
writePinActiveLow :: Pin -> Bool -> IO ()
writePinActiveLow p v = BS.writeFile (pinActiveLowFileName p) (toSysfsActiveLowValue v)

-- | Return a list of all pins that are exposed via the 'sysfs' GPIO
-- filesystem. Note that the returned list may omit some pins that
-- are available on the host but which, for various reasons, are not
-- exposed via the 'sysfs' GPIO filesystem.
availablePins :: IO [Pin]
availablePins =
  do sysfsEntries <- getDirectoryContents sysfsPath
     let sysfsContents = fmap (sysfsPath </>) sysfsEntries
     sysfsDirectories <- filterM doesDirectoryExist sysfsContents
     let chipDirs = filter (isPrefixOf "gpiochip" . takeFileName) sysfsDirectories
     pins <- mapM pinRange chipDirs
     return $ sort $ concat pins

-- Helper functions that aren't exported.
--

intToBS :: Int -> ByteString
intToBS = LBS.toStrict . toLazyByteString . intDec

toSysfsPinDirection :: PinDirection -> ByteString
toSysfsPinDirection In = "in"
toSysfsPinDirection Out = "out"

toSysfsPinEdge :: SysfsEdge -> ByteString
toSysfsPinEdge None = "none"
toSysfsPinEdge Rising = "rising"
toSysfsPinEdge Falling = "falling"
toSysfsPinEdge Both = "both"

toSysfsPinValue :: PinValue -> ByteString
toSysfsPinValue Low = "0"
toSysfsPinValue High = "1"

toSysfsActiveLowValue :: Bool -> ByteString
toSysfsActiveLowValue False = "0"
toSysfsActiveLowValue True = "1"

readIntFromFile :: FilePath -> IO Int
readIntFromFile f =
  do contents <- BS.readFile f
     case C8.readInt contents of
       Just (n, _) -> return n
       Nothing -> throwM $ UnexpectedContents f (C8.unpack contents)

chipBaseGpio :: FilePath -> IO Int
chipBaseGpio chipDir = readIntFromFile (chipDir </> "base")

chipNGpio :: FilePath -> IO Int
chipNGpio chipDir = readIntFromFile (chipDir </> "ngpio")

pinRange :: FilePath -> IO [Pin]
pinRange chipDir =
  do base <- chipBaseGpio chipDir
     ngpio <- chipNGpio chipDir
     if base >= 0 && ngpio > 0
        then return $ fmap Pin [base .. (base + ngpio - 1)]
        else return []
