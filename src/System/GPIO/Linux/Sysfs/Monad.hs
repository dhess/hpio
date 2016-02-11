{-|
Module      : System.GPIO.Linux.Sysfs.Monad
Description : A monad type class for Linux @sysfs@ GPIO operations
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

A monad type class for Linux @sysfs@ GPIO operations.

-}

{-# LANGUAGE Safe #-}

module System.GPIO.Linux.Sysfs.Monad
       ( -- * MonadSysfs class
         MonadSysfs(..)
       ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)
import Data.ByteString (ByteString)
import Data.Monoid (Monoid)
import Foreign.C.Types (CInt(..))

-- | A type class for monads which implement (or mock) low-level Linux
-- @sysfs@ GPIO operations.
--
-- This type class chiefly exists in order to use a @sysfs@ mock for
-- testing with the 'System.GPIO.Linux.Sysfs.SysfsT'
-- transformer/interpreter (see
-- 'System.GPIO.Linux.Sysfs.Mock.SysfsMockT'). Typically, you will not
-- need to use it directly. If you want to write Linux @sysfs@ GPIO
-- programs that run in 'IO', see the "System.GPIO.Linux.Sysfs.IO"
-- module.
--
-- To see the documentation for this type class's methods, see the
-- canonical implementations of these methods in
-- "System.GPIO.Linux.Sysfs.IO". (The low-level GPIO functions in that
-- module have the same name as the methods in this type class.)
class (Monad m) => MonadSysfs m where
  doesDirectoryExist :: FilePath -> m Bool
  doesFileExist :: FilePath -> m Bool
  getDirectoryContents :: FilePath -> m [FilePath]
  readFile :: FilePath -> m ByteString
  writeFile :: FilePath -> ByteString -> m ()
  unlockedWriteFile :: FilePath -> ByteString -> m ()
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
