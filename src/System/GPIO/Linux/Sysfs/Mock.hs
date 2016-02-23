{-|
Module      : System.GPIO.Linux.Sysfs.Mock
Description : A mock MonadSysfs instance.
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

A mock 'MonadSysfs' instance, for testing GPIO programs.

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module System.GPIO.Linux.Sysfs.Mock
       ( -- * The SysfsMock monad
         SysfsMockT(..)
       , runSysfsMockT
       , runSysfsMock
       , evalSysfsMock
       , execSysfsMock
         -- * SysfsMock types
       , MockPinState(..)
       , defaultState
       , MockGpioChip(..)
         -- * Mock @sysfs@ operations
       , doesDirectoryExist
       , doesFileExist
       , getDirectoryContents
       , readFile
       , writeFile
       , unlockedWriteFile
       , pollFile
         -- * A mock @sysfs@ filesystem
       , sysfsRoot
         -- * Mock @sysfs@ exceptions
       , MockFSException(..)
       ) where

import Prelude hiding (readFile, writeFile)
import Control.Applicative ((<$>), Alternative, Applicative)
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.Catch.Pure (Catch, runCatch)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State.Strict
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8 (lines, unlines)
import Data.Data
import Data.Either (isRight)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust, isJust, maybe)
import Data.Tuple (fst, snd)
import Foreign.C.Types (CInt(..))
import System.FilePath (splitFileName)
import System.GPIO.Linux.Sysfs.Free (SysfsT)
import System.GPIO.Linux.Sysfs.Mock.Internal
import System.GPIO.Linux.Sysfs.Monad (MonadSysfs)
import qualified System.GPIO.Linux.Sysfs.Monad as M (MonadSysfs(..))
import System.GPIO.Linux.Sysfs.Types (SysfsEdge(..))
import System.GPIO.Types (PinDirection(..), PinValue(..))

-- | A mock pin.
--
-- Note that in the real Linux @sysfs@, pins keep their state even
-- after they're unexported.
data MockPinState =
  MockPinState {_direction :: Maybe PinDirection
               ,_activeLow :: !Bool
               ,_value :: !PinValue -- This is the line level
               ,_edge :: Maybe SysfsEdge}
  deriving (Show,Eq)

-- | Default initial state of mock pins.
defaultState :: MockPinState
defaultState =
  MockPinState {_direction = Just Out
               ,_activeLow = False
               ,_value = Low
               ,_edge = Just None}

-- | A mock "gpiochip".
--
-- Note that the '_initialPinStates' list is only used to construct a
-- mock filesystem. For each 'MockPinState' value in the list, a mock
-- pin is created in the mock filesystem with the path
-- @/sys/class/gpio/gpioN@, where @N@ is @_base@ + the pin's index in
-- the '_initialPinStates' list.
data MockGpioChip =
  MockGpioChip {_label :: String
               ,_base :: Int
               ,_initialPinStates :: [MockPinState]}
  deriving (Show,Eq)

-- | A monad transformer which adds mock @sysfs@ computations to an
-- inner monad 'm'.
newtype SysfsMockT m a =
  SysfsMockT {unSysfsMockT :: StateT MockFSZipper m a}
  deriving (Alternative,Applicative,Functor,Monad,MonadFix,MonadIO,MonadThrow,MonadCatch,MonadMask,MonadState MockFSZipper,MonadReader r,MonadWriter w)

-- | Run a mock @sysfs@ computation in monad 'm' with the given
-- 'MockFSZipper', and return a tuple containing the computation's
-- value and the final 'MockFSZipper' state.
runSysfsMockT :: (Monad m) => SysfsMockT m a -> MockFSZipper -> m (a, MockFSZipper)
runSysfsMockT action = runStateT (unSysfsMockT action)

-- | The simplest possible (pure) mock @sysfs@ monad.
type SysfsMock a = SysfsMockT Catch a

-- | Run a 'SysfsMock' computation with the given 'MockFSZipper', and
-- return a tuple containing the computation's value and the final
-- 'MockFSZipper' state.
runSysfsMock :: SysfsMock a -> MockFSZipper -> Either MockFSException (a, MockFSZipper)
runSysfsMock a z =
  -- The 'MonadThrow' instance for 'Either' 'e' requires that 'e' '~'
  -- 'SomeException', and 'SomeException' has no 'Eq' instance, which
  -- makes this monad not very useful for testing. Therefore, we convert the
  -- exception type back to 'MockFSException'.
  case runCatch $ runSysfsMockT a z of
    Right result -> return result
    Left e ->
      -- Should be safe as there's no other exception type in this
      -- stack.
      Left $ fromJust $ fromException e

-- | Run a 'SysfsMock' computation with the given 'MockFSZipper', and
-- return the computation's value, discarding the final state.
evalSysfsMock :: SysfsMock a -> MockFSZipper -> Either MockFSException a
evalSysfsMock a z = fst <$> runSysfsMock a z

-- | Run a 'SysfsMock' computation with the given 'MockFSZipper', and
-- return the final 'MockFSZipper' state, discarding the computation's
-- value.
execSysfsMock :: SysfsMock a -> MockFSZipper -> Either MockFSException MockFSZipper
execSysfsMock a z = snd <$> runSysfsMock a z

instance (MonadSysfs m, MonadThrow m) => M.MonadSysfs (SysfsMockT m) where
  doesDirectoryExist = doesDirectoryExist
  doesFileExist = doesFileExist
  getDirectoryContents = getDirectoryContents
  readFile = readFile
  writeFile = writeFile
  unlockedWriteFile = unlockedWriteFile
  pollFile = pollFile

mcd :: (Monad m) => FilePath -> SysfsMockT m (Either MockFSException MockFSZipper)
mcd dirName =
  do fsz <- get
     return $ cd dirName fsz

doesDirectoryExist :: (Monad m) => FilePath -> SysfsMockT m Bool
doesDirectoryExist path =
  mcd path >>= \case
    Left _ -> return False
    Right _ -> return True

doesFileExist :: (Monad m) => FilePath -> SysfsMockT m Bool
doesFileExist path =
  let (dirName, fileName) = splitFileName path
  in
    mcd dirName >>= \case
      Left _ -> return False
      Right (parent, _) ->
        return (isJust $ findFile' fileName parent)

getDirectoryContents :: (MonadThrow m) => FilePath -> SysfsMockT m [FilePath]
getDirectoryContents path =
  mcd path >>= \case
    Left e -> throwM e
    Right (parent, _) ->
      return $ fmap _dirName (_subdirs parent) ++ fmap _fileName (_files parent)

readFile :: (MonadThrow m) => FilePath -> SysfsMockT m ByteString
readFile path =
  let (dirName, fileName) = splitFileName path
  in
    mcd dirName >>= \case
      Left  e -> throwM e
      Right (parent, _) ->
        case findFile' fileName parent of
          Nothing -> throwM $ NotAFile path
          Just file -> return $ C8.unlines $ _contents file

writeFile :: (MonadThrow m) => FilePath -> ByteString -> SysfsMockT m ()
writeFile = undefined

unlockedWriteFile :: (MonadThrow m) => FilePath -> ByteString -> SysfsMockT m ()
unlockedWriteFile = writeFile

pollFile :: (Monad m) => FilePath -> Int -> SysfsMockT m CInt
pollFile _ _ = return 1

sysfsRoot :: Directory
sysfsRoot =
  Directory "/"
            []
            [Directory "sys"
                       []
                       [Directory "class"
                                  []
                                  [Directory "gpio"
                                             [File "export" ["Export"]
                                             ,File "unexport" ["Unexport"]]
                                             []]]]
