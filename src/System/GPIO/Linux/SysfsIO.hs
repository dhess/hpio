-- | The actual Linux 'sysfs' GPIO implementation. This implementation
-- will only function properly on Linux systems with a 'sysfs' GPIO
-- subsystem, obviously.
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module System.GPIO.Linux.SysfsIO
         ( -- * SysfsIOT transformer
           SysfsIOT(..)
         , SysfsIO
         , runSysfsIO
         ) where

import Prelude hiding (readFile, writeFile)
import Control.Applicative
import Control.Error.Script (scriptIO)
import Control.Error.Util (hushT)
import Control.Monad (filterM)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Char (toLower)
import Data.List (isPrefixOf, sort)
import Data.Maybe (catMaybes)
import Foreign.C.Error (throwErrnoIfMinus1Retry_)
import Foreign.C.Types (CInt(..))
import qualified Language.C.Inline as C (include)
import qualified Language.C.Inline.Interruptible as CI
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName)
import System.GPIO.Linux.MonadSysfs
import System.GPIO.Linux.Sysfs
import System.GPIO.Types
import qualified System.IO as IO (writeFile)
import qualified System.IO.Strict as IOS (readFile)
import System.Posix.IO (OpenMode(ReadOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (Fd)

-- Our poll(2) wrapper.
--
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
pollSysfs :: Fd -> IO CInt
pollSysfs fd =
  let cfd = fromIntegral fd
  in
    [CI.block| int {
         uint8_t dummy;
         if (read($(int cfd), &dummy, 1) == -1) {
             return -1;
         }

         struct pollfd fds = { .fd = $(int cfd), .events = POLLPRI|POLLERR, .revents = 0 };
         if (poll(&fds, 1, -1) == -1)  {
             return -1;
         }
         if (lseek(fds.fd, 0, SEEK_SET) == -1) {
             return -1;
         }
         return 0;
     } |]

-- | A monad transformer which adds Linux 'sysfs' GPIO computations to
-- an inner monad 'm'.
newtype SysfsIOT m a =
  SysfsIOT { runSysfsIOT :: m a }
  deriving (Alternative,Applicative,Functor,Monad,MonadIO,MonadThrow,MonadCatch,MonadMask)

-- | A convenient specialization of 'SysfsT' which runs GPIO
-- computations in 'IO'.
type SysfsIO a = SysfsT (SysfsIOT IO) (SysfsIOT IO) a

-- | Run a 'SysfsIO' computation in the 'IO' monad and return the
-- result.
runSysfsIO :: SysfsIO a -> IO a
runSysfsIO action = runSysfsIOT $ runSysfsT action

instance (MonadIO m, MonadThrow m) => MonadSysfs (SysfsIOT m) where
  sysfsIsPresent = sysfsIsPresentIO
  pinIsExported = pinIsExportedIO
  pinHasDirection = pinHasDirectionIO
  pinHasEdge = pinHasEdgeIO
  exportPin = exportPinIO
  unexportPin = unexportPinIO
  readPinDirection = readPinDirectionIO
  writePinDirection = writePinDirectionIO
  writePinDirectionWithValue = writePinDirectionWithValueIO
  readPinValue = readPinValueIO
  threadWaitReadPinValue = threadWaitReadPinValueIO
  writePinValue = writePinValueIO
  readPinEdge = readPinEdgeIO
  writePinEdge = writePinEdgeIO
  readPinActiveLow = readPinActiveLowIO
  writePinActiveLow = writePinActiveLowIO
  availablePins = availablePinsIO


-- Helper functions that aren't exported.
--

sysfsIsPresentIO :: (MonadIO m) => m Bool
sysfsIsPresentIO = liftIO $ doesDirectoryExist sysfsPath

pinIsExportedIO :: (MonadIO m) => Pin -> m Bool
pinIsExportedIO p = liftIO $ doesDirectoryExist (pinDirName p)

pinHasDirectionIO :: (MonadIO m) => Pin -> m Bool
pinHasDirectionIO p = liftIO $ doesFileExist (pinDirectionFileName p)

pinHasEdgeIO :: (MonadIO m) => Pin -> m Bool
pinHasEdgeIO p = liftIO $ doesFileExist (pinEdgeFileName p)

exportPinIO :: (MonadIO m) => Pin -> m ()
exportPinIO (Pin n) = liftIO $ IO.writeFile exportFileName (show n)

unexportPinIO :: (MonadIO m) => Pin -> m ()
unexportPinIO (Pin n) = liftIO $ IO.writeFile unexportFileName (show n)

readPinDirectionIO :: (MonadIO m, MonadThrow m) => Pin -> m PinDirection
readPinDirectionIO p =
  liftIO $ IOS.readFile (pinDirectionFileName p) >>= \case
    "in\n"  -> return In
    "out\n" -> return Out
    _     -> throwM $ UnexpectedDirection p

writePinDirectionIO :: (MonadIO m) => Pin -> PinDirection -> m ()
writePinDirectionIO p d = liftIO $ IO.writeFile (pinDirectionFileName p) (lowercase $ show d)

writePinDirectionWithValueIO :: (MonadIO m) => Pin -> PinValue -> m ()
writePinDirectionWithValueIO p v = liftIO $ IO.writeFile (pinDirectionFileName p) (lowercase $ show v)

readPinValueIO :: (MonadIO m, MonadThrow m) => Pin -> m PinValue
readPinValueIO p =
  liftIO $ IOS.readFile (pinValueFileName p) >>= \case
    "0\n" -> return Low
    "1\n" -> return High
    _   -> throwM $ UnexpectedValue p

threadWaitReadPinValueIO :: (MonadIO m) => Pin -> m PinValue
threadWaitReadPinValueIO p = liftIO $
  do fd <- openFd (pinValueFileName p) ReadOnly Nothing defaultFileFlags
     throwErrnoIfMinus1Retry_ "pollSysfs" $ pollSysfs fd
     -- Could use fdRead here and handle EAGAIN, but it's easier to
     -- close the fd and use nice handle-based IO, instead. If this
     -- becomes a performance problem... we probably need a
     -- non-sysfs-based interpreter in that case, anyway.
     closeFd fd
     readPinValueIO p

writePinValueIO :: (MonadIO m) => Pin -> PinValue -> m ()
writePinValueIO p v = liftIO $ IO.writeFile (pinValueFileName p) (toSysfsPinValue v)

readPinEdgeIO :: (MonadIO m) => Pin -> m PinReadTrigger
readPinEdgeIO p =
  liftIO $ IOS.readFile (pinEdgeFileName p) >>= \case
    "none\n"  -> return Disabled
    "rising\n" -> return RisingEdge
    "falling\n" -> return FallingEdge
    "both\n" -> return Level
    _     -> throwM $ UnexpectedEdge p

writePinEdgeIO :: (MonadIO m) => Pin -> PinReadTrigger -> m ()
writePinEdgeIO p v = liftIO $ IO.writeFile (pinEdgeFileName p) (toSysfsPinEdge v)

readPinActiveLowIO :: (MonadIO m) => Pin -> m Bool
readPinActiveLowIO p =
  liftIO $ IOS.readFile (pinActiveLowFileName p) >>= \case
    "0\n" -> return False
    "1\n" -> return True
    _   -> throwM $ UnexpectedActiveLow p

writePinActiveLowIO :: (MonadIO m) => Pin -> Bool -> m ()
writePinActiveLowIO p v = liftIO $ IO.writeFile (pinActiveLowFileName p) (toSysfsActiveLowValue v)

availablePinsIO :: (MonadIO m) => m [Pin]
availablePinsIO =
  do sysfsEntries <- liftIO $ getDirectoryContents sysfsPath
     let sysfsContents = fmap (sysfsPath </>) sysfsEntries
     sysfsDirectories <- filterM (liftIO . doesDirectoryExist) sysfsContents
     let chipDirs = filter (\f -> isPrefixOf "gpiochip" $ takeFileName f) sysfsDirectories
     maybePins <- mapM (runMaybeT . pinRange) chipDirs
     return $ sort $ concat $ catMaybes maybePins

lowercase :: String -> String
lowercase = fmap toLower

toSysfsPinEdge :: PinReadTrigger -> String
toSysfsPinEdge Disabled = "none"
toSysfsPinEdge RisingEdge = "rising"
toSysfsPinEdge FallingEdge = "falling"
toSysfsPinEdge Level = "both"

toSysfsPinValue :: PinValue -> String
toSysfsPinValue Low = "0"
toSysfsPinValue High = "1"

toSysfsActiveLowValue :: Bool -> String
toSysfsActiveLowValue False = "0"
toSysfsActiveLowValue True = "1"

readFromFile :: (MonadIO m, Read a) => FilePath -> m a
readFromFile f = liftIO (IOS.readFile f >>= readIO)

maybeIO :: (MonadIO m) => IO a -> MaybeT m a
maybeIO = hushT . scriptIO

chipBaseGpio :: (MonadIO m) => FilePath -> m Int
chipBaseGpio chipDir = readFromFile (chipDir </> "base")

chipNGpio :: (MonadIO m) => FilePath -> m Int
chipNGpio chipDir = readFromFile (chipDir </> "ngpio")

pinRange :: (MonadIO m) => FilePath -> MaybeT m [Pin]
pinRange chipDir =
  do base <- maybeIO $ chipBaseGpio chipDir
     ngpio <- maybeIO $ chipNGpio chipDir
     case (base >= 0 && ngpio > 0) of
       False -> return []
       True -> return $ fmap Pin [base .. (base + ngpio - 1)]
