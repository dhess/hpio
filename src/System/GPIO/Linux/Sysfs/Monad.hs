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
