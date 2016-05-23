{-|
Module      : System.GPIO.Linux.Sysfs.IO
Description : Linux @sysfs@ GPIO operations in IO
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

The actual Linux @sysfs@ implementation. This implementation will only
function properly on Linux systems with a @sysfs@ subsystem,
obviously.

-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Trustworthy #-}

module System.GPIO.Linux.Sysfs.IO
         ( -- * SysfsIOT transformer
           SysfsIOT(..)
         ) where

import Prelude ()
import Prelude.Compat
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, bracket)
import Control.Monad.Cont (MonadCont)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile, writeFile)
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.C.Types (CInt(..))
import qualified System.Directory as D (doesDirectoryExist, doesFileExist, getDirectoryContents)
import "unix" System.Posix.IO (OpenMode(ReadOnly, WriteOnly), closeFd, defaultFileFlags, openFd)
import "unix-bytestring" System.Posix.IO.ByteString (fdWrite)

import System.GPIO.Linux.Sysfs.Monad (MonadSysfs(..))

-- | An instance of 'MonadSysfs' which runs 'MonadSysfs' operations in
-- IO. This instance must be run on an actual Linux @sysfs@ GPIO
-- filesystem and will fail in any other environment.
--
-- == Interactions with threads
--
-- Some parts of this implementation use the Haskell C FFI, and may
-- block on C I/O operations. (Specifically, 'pollFile' will block in
-- the C FFI until its event is triggered.) When using this
-- implementation with GHC, you should compile your program with the
-- @-threaded@ option, so that threads performing these blocking
-- operations do not block other Haskell threads in the system.
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
  deriving (Functor,Alternative,Applicative,Monad,MonadFix,MonadPlus,MonadThrow,MonadCatch,MonadMask,MonadCont,MonadIO,MonadReader r,MonadError e,MonadWriter w,MonadState s,MonadRWS r w s)

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

unlockedWriteFileIO :: FilePath -> ByteString -> IO ()
unlockedWriteFileIO fn bs =
  bracket
    (openFd fn WriteOnly Nothing defaultFileFlags)
    closeFd
    (\fd -> void $ fdWrite fd bs)

foreign import ccall interruptible "pollSysfs" pollSysfs :: CInt -> CInt -> IO CInt

pollFileIO :: FilePath -> Int -> IO CInt
pollFileIO fn timeout =
  bracket
    (openFd fn ReadOnly Nothing defaultFileFlags)
    closeFd
    (\fd -> throwErrnoIfMinus1Retry "pollSysfs" $ pollSysfs (fromIntegral fd) (fromIntegral timeout))
