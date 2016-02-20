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
       , runSysfsMockT
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
       , up
       , cd
       , root
       , findFile
       , findFile'
       , findDir
       , findDir'
       , mkdir
       , mkfile
       , rmdir
       , rmfile
         -- * Mock filesystem types
       , Name
       , File(..)
       , Directory(..)
       , MockFSException(..)
       , MockFSCrumb(..)
       , MockFSZipper
       ) where

import Prelude hiding (readFile, writeFile)
import Control.Applicative (Alternative, Applicative)
import Control.Monad (void)
import Control.Monad.Catch
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
import Data.Foldable (foldlM)
import Data.List (break, delete, find)
import Data.Maybe (isJust, maybe)
import Foreign.C.Types (CInt(..))
import GHC.Generics
import System.FilePath (isAbsolute, isValid, splitDirectories, splitFileName)
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
  SysfsMockT {unSysfsMockT :: StateT MockFSZipper m a}
  deriving (Alternative,Applicative,Functor,Monad,MonadFix,MonadIO,MonadThrow,MonadCatch,MonadMask,MonadState MockFSZipper,MonadReader r,MonadWriter w)

-- | Run a mock @sysfs@ computation with the given 'MockFSZipper', and
-- return a tuple containing the computation's value and the final
-- 'MockFSZipper' state.
runSysfsMockT :: (Monad m) => SysfsMockT m a -> MockFSZipper -> m (a, MockFSZipper)
runSysfsMockT action = runStateT (unSysfsMockT action)

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

type Name = String

data File =
  File {_fileName :: Name
       ,_contents :: [ByteString]}
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
  | FileExists Name
  | InvalidName Name
  deriving (Show,Eq,Typeable)

instance Exception MockFSException

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

findFile :: Name -> Directory -> ([File], [File])
findFile name cwd = break (\file -> _fileName file == name) (_files cwd)

findFile' :: Name -> Directory -> Maybe File
findFile' name cwd = find (\file -> _fileName file == name) (_files cwd)

findDir :: Name -> Directory -> ([Directory], [Directory])
findDir name cwd = break (\dir -> _dirName dir == name) (_subdirs cwd)

findDir' :: Name -> Directory -> Maybe Directory
findDir' name cwd = find (\dir -> _dirName dir == name) (_subdirs cwd)

isValidName :: Name -> Bool
isValidName name = isValid name && notElem '/' name

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
              case findDir name cwd of
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
                        (findFile' name cwd)

mkdir :: Name -> MockFSZipper -> Either MockFSException MockFSZipper
mkdir name (parent, bs) =
  if (isJust $ findFile' name parent)
    then Left $ FileExists name
    else
      case findDir name parent of
        (_, []) ->
          if isValidName name
            then
              let child = Directory name [] []
                  subdirs = _subdirs parent
              in
                Right $ (parent { _subdirs = (child:subdirs)}, bs)
            else Left $ InvalidName name
        _ -> Left $ FileExists name

mkfile :: Name -> [ByteString] -> Bool -> MockFSZipper -> Either MockFSException MockFSZipper
mkfile name contents clobber (parent, bs) =
  case findFile name parent of
    (ls, _:rs) ->
      if clobber
         then mkfile' $ ls ++ rs
         else Left $ FileExists name
    _ ->
      maybe (mkfile' $ _files parent)
            (const $ Left (FileExists name))
            (findDir' name parent)
  where
    mkfile' :: [File] -> Either MockFSException MockFSZipper
    mkfile' files =
      if isValidName name
        then
          let file = File name contents
          in
            Right $ (parent { _files = (file:files)}, bs)
        else Left $ InvalidName name

rmfile :: Name -> MockFSZipper -> Either MockFSException MockFSZipper
rmfile name (parent, bs) =
  if (isJust $ findDir' name parent)
     then Left $ NotAFile name
     else
       case findFile name parent of
         (ls, _:rs) -> Right $ (parent {_files = ls ++ rs}, bs)
         _ -> Left $ NoSuchFileOrDirectory name

-- Note: recursive!
rmdir :: Name -> MockFSZipper -> Either MockFSException MockFSZipper
rmdir name (parent, bs) =
  if (isJust $ findFile' name parent)
     then Left $ NotADirectory name
     else
       case findDir name parent of
         (ls, _:rs) -> Right $ (parent {_subdirs = ls ++ rs}, bs)
         _ -> Left $ NoSuchFileOrDirectory name

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
