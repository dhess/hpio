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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module System.GPIO.Linux.Sysfs.Mock
       ( -- * The SysfsMock monad
         SysfsMockT(..)
         -- * SysfsMock types
       , MockPinState(..)
       , defaultState
       , MockGpioChip(..)
       , MockFS
         -- * Mock @sysfs@ operations
       , doesDirectoryExist
       , doesFileExist
       , getDirectoryContents
       , readFile
       , writeFile
       , unlockedWriteFile
       , pollFile
       ) where

import Prelude hiding (readFile, writeFile)
import Control.Applicative (Applicative)
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.ByteString (ByteString)
import Data.Data
import Data.Foldable (foldlM)
import Data.List (break)
import Foreign.C.Types (CInt(..))
import GHC.Generics
import System.FilePath (splitDirectories)
import System.GPIO.Linux.Sysfs.Free (SysfsT)
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
  SysfsMockT {runSysfsMockT :: m a}
  deriving (Applicative,Functor,Monad,MonadFix,MonadIO,MonadThrow,MonadCatch,MonadMask)

instance MonadTrans SysfsMockT where
  lift = SysfsMockT

instance (MonadSysfs m) => M.MonadSysfs (SysfsMockT m) where
  doesDirectoryExist = doesDirectoryExist
  doesFileExist = doesFileExist
  getDirectoryContents = getDirectoryContents
  readFile = readFile
  writeFile = writeFile
  unlockedWriteFile = unlockedWriteFile
  pollFile = pollFile

doesDirectoryExist :: (MonadSysfs m) => FilePath -> m Bool
doesDirectoryExist fn = return False

doesFileExist :: (MonadSysfs m) => FilePath -> m Bool
doesFileExist fn = return False

getDirectoryContents :: (MonadSysfs m) => FilePath -> m [FilePath]
getDirectoryContents fn = return []

readFile :: (MonadSysfs m) => FilePath -> m ByteString
readFile fn = return ""

writeFile :: (MonadSysfs m) => FilePath -> ByteString -> m ()
writeFile fn bs = undefined

unlockedWriteFile :: (MonadSysfs m) => FilePath -> ByteString -> m ()
unlockedWriteFile = writeFile

pollFile :: (MonadSysfs m) => FilePath -> Int -> m CInt
pollFile fn timeout = undefined

type Name = String

data MockFS
  = Directory Name [MockFS]
  | File Name [String]
  deriving (Show, Eq)

data MockFSException
  = ReadError FilePath
  | WriteError FilePath
  | NotADirectory FilePath
  | NotAFile FilePath
  | NoSuchFileOrDirectory FilePath
  deriving (Show,Typeable)

data MockFSCrumb
  = MockFSCrumb Name [MockFS] [MockFS] deriving (Show, Eq)

fsName :: MockFS -> Name
fsName (Directory n _) = n
fsName (File n _) = n

type MockFSZipper = (MockFS, [MockFSCrumb])

isDirectory :: MockFSZipper -> Bool
isDirectory (Directory _ _, _) = True
isDirectory _ = False

isFile :: MockFSZipper -> Bool
isFile (File _ _, _) = True
isFile _ = False

-- Logically equivalent to "cd .."
up :: MockFSZipper -> Either MockFSException MockFSZipper
up (item, MockFSCrumb parent ls rs:bs) = Right (Directory parent (ls ++ [item] ++ rs), bs)
up (item, []) = Right (item, []) -- cd /.. == /

goto :: Name -> MockFSZipper -> Either MockFSException MockFSZipper
goto name dir@(Directory dirName items, bs) =
  case name of
    "." -> Right dir
    ".." -> up dir
    _ ->
      case break (\item -> fsName item == name) items of
        (ls, item:rs) -> Right (item, MockFSCrumb dirName ls rs:bs)
        (_, []) -> Left $ NoSuchFileOrDirectory name
goto _ (File fileName _, _) = Left $ NotADirectory fileName

cd :: FilePath -> MockFSZipper -> Either MockFSException MockFSZipper
cd path zipper =
  let
    result = foldlM (flip goto) zipper (splitDirectories path)
  in
    case result of
      Right dir@ (Directory _ _, _) -> Right dir
      _ -> Left $ NotADirectory path

sysfsRoot :: MockFS
sysfsRoot =
  Directory "."
            [Directory "/"
                       [Directory "sys"
                                  [Directory "class"
                                             [Directory "gpio"
                                                        [File "export" ["Export"]
                                                        ,File "unexport" ["Unexport"]]]]]]
