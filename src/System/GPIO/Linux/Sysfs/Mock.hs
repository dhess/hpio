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
import Data.List (break, find)
import Data.Maybe (maybe)
import Foreign.C.Types (CInt(..))
import GHC.Generics
import System.FilePath (isAbsolute, splitDirectories)
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

data File =
  File {_fileName :: Name
       ,_contents :: [String]}
  deriving (Show,Eq)

data Directory =
  Directory {_dirName :: Name
            ,_files :: [File]
            ,_subdirs :: [Directory]}
  deriving (Show,Eq)

data MockFSException
  = ReadError FilePath
  | WriteError FilePath
  | NotADirectory FilePath
  | NotAFile FilePath
  | NoSuchFileOrDirectory FilePath
  deriving (Show,Typeable)

data MockFSCrumb =
  MockFSCrumb {_parentName :: Name
              ,_parentFiles :: [File]
              ,_pred :: [Directory]
              ,_succ :: [Directory]}
  deriving (Show,Eq)

type MockFSZipper = (Directory, [MockFSCrumb])

-- Logically equivalent to "cd .."
up :: MockFSZipper -> MockFSZipper
up (cwd, MockFSCrumb parent files ls rs:bs) = (Directory parent files (ls ++ [cwd] ++ rs), bs)
up (cwd, []) = (cwd, []) -- cd /.. == /

root :: MockFSZipper -> MockFSZipper
root (top, []) = (top, [])
root z = root $ up z

findFile :: Name -> Directory -> Maybe File
findFile name dir = find (\file -> _fileName file == name) (_files dir)

cd :: FilePath -> MockFSZipper -> Either MockFSException MockFSZipper
cd p z =
  let (path, fs) =
        if isAbsolute p
           then (drop 1 p, root z)
           else (p, z)
  in foldlM cd' fs (splitDirectories path)
  where cd' :: MockFSZipper -> Name -> Either MockFSException MockFSZipper
        cd' zipper@(cwd,bs) name =
          case name of
            "." -> Right zipper
            ".." -> return $ up zipper
            _ ->
              case break (\dir -> _dirName dir == name)
                         (_subdirs cwd) of
                (ls,subdir:rs) ->
                  Right (subdir
                        ,MockFSCrumb (_dirName cwd)
                                     (_files cwd)
                                     ls
                                     rs :
                         bs)
                (_,[]) ->
                  maybe (Left $ NoSuchFileOrDirectory p)
                        (const $ Left $ NotADirectory p)
                        (findFile name cwd)

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
