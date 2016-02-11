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
         ) where

import Control.Applicative (Applicative)
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile, writeFile)
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.C.Types (CInt(..))
import qualified Language.C.Inline as C (include)
import qualified Language.C.Inline.Interruptible as CI
import qualified System.Directory as D (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.GPIO.Linux.Sysfs.Monad
import System.GPIO.Linux.Sysfs.Free
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

instance (MonadIO m, MonadThrow m) => MonadSysfs (SysfsIOT m) where
  doesDirectoryExist = liftIO . D.doesDirectoryExist
  doesFileExist = liftIO . D.doesFileExist
  getDirectoryContents = liftIO . D.getDirectoryContents
  readFile = liftIO . BS.readFile
  writeFile fn bs = liftIO $ BS.writeFile fn bs
  unlockedWriteFile fn bs = liftIO $ unlockedWriteFileIO fn bs
  pollFile fn timeout = liftIO $ pollFileIO fn timeout

-- @sysfs@ control files which are global shared resources may be
-- written simultaneously by multiple threads. This is fine -- @sysfs@
-- can handle this -- but Haskell's writeFile cannot, as it locks the
-- file and prevents multiple writers. We don't want this behavior, so
-- we use low-level operations to get around it.
unlockedWriteFileIO :: FilePath -> ByteString -> IO ()
unlockedWriteFileIO fn bs =
  bracket
    (openFd fn WriteOnly Nothing defaultFileFlags)
    closeFd
    (\fd -> void $ fdWrite fd bs)

pollFileIO :: FilePath -> Int -> IO CInt
pollFileIO fn timeout =
  bracket
    (openFd fn ReadOnly Nothing defaultFileFlags)
    closeFd
    (\fd -> throwErrnoIfMinus1Retry "pollSysfs" $ pollSysfs fd timeout)
