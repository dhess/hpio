{-|
Module      : System.GPIO.Linux.Sysfs.Mock.Internal
Description : Functions used by the mock MonadSysfs instance.
Copyright   : (c) 2017, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <dhess-src@quixoftic.com>
Stability   : experimental
Portability : non-portable

Types and functions to emulate a (pure) rudimentary Posix-style
filesystem.

This module was written for internal use only. Its interface may
change at any time. Documentation in this module is sparse, by design.

N.B.: This mock filesystem implementation was written with the
intention of doing only just enough to emulate the operations needed
by the 'System.GPIO.Linux.Sysfs.Monad.MonadSysfs' type class. Though
it may be possible to use this implementation for other purposes, it
has neither been designed nor tested for that. Use at your own risk
and please do not submit requests for addtional functionality.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module System.GPIO.Linux.Sysfs.Mock.Internal
       ( -- * Mock filesystem types
         Name
       , File(..)
       , FileType(..)
       , DirNode(..)
       , Directory
       , directory
       , dirName
       , files
       , dirNode
       , subdirs
       , MockFSCrumb(..)
       , MockFSZipper(..)
         -- * Mock filesystem operations
       , cd
       , pathFromRoot
       , findFile
       , mkdir
       , mkfile
       , rmdir
       , rmfile
       ) where

import Protolude
import Data.Tree (Tree(..))
import GHC.IO.Exception (IOErrorType(..))
import System.FilePath (isAbsolute, isValid, joinPath, splitDirectories)
import System.IO.Error (IOError, mkIOError)

import System.GPIO.Types (Pin)

type Name = FilePath

data FileType
  = Constant [ByteString]
  | Export
  | Unexport
  | Value Pin
  | Direction Pin
  | Edge Pin
  | ActiveLow Pin
  deriving (Show,Eq)

data File =
  File {_fileName :: !Name
       ,_fileType :: !FileType}
  deriving (Show,Eq)

data DirNode =
  DirNode {_dirNodeName :: !Name
          ,_files :: [File]}
  deriving (Show,Eq)

type Directory = Tree DirNode

-- Getters.

directory :: Name -> [File] -> [Directory] -> Directory
directory name fs = Node (DirNode name fs)

dirName :: Directory -> Name
dirName = _dirNodeName . dirNode

files :: Directory -> [File]
files = _files . dirNode

dirNode :: Directory -> DirNode
dirNode = rootLabel

subdirs :: Directory -> [Directory]
subdirs = subForest

data MockFSCrumb =
  MockFSCrumb {_node :: DirNode
              ,_pred :: [Directory]
              ,_succ :: [Directory]}
  deriving (Show,Eq)

-- | An opaque type representing the current state of the mock @sysfs@
-- filesystem. Because the constructor is not exported via the public
-- interface, you cannot create these directly, but you can manipulate
-- them using the exposed mock @sysfs@ operations and then pass those
-- 'MockFSZipper's around.
data MockFSZipper =
  MockFSZipper {_cwd :: Directory
               ,_crumbs :: [MockFSCrumb]}
  deriving (Show,Eq)

-- Logically equivalent to "cd .."
up :: MockFSZipper -> MockFSZipper
up (MockFSZipper dir (MockFSCrumb parent ls rs:bs)) =
  MockFSZipper (directory (_dirNodeName parent) (_files parent) (ls ++ [dir] ++ rs))
               bs
up (MockFSZipper dir []) = MockFSZipper dir [] -- cd /.. == /

root :: MockFSZipper -> MockFSZipper
root (MockFSZipper t []) = MockFSZipper t []
root z = root $ up z

pathFromRoot :: MockFSZipper -> FilePath
pathFromRoot zipper =
  joinPath $ "/" : reverse (unfoldr up' zipper)
  where
    up' :: MockFSZipper -> Maybe (Name, MockFSZipper)
    up' z@(MockFSZipper dir (_:_)) = Just (dirName dir, up z)
    up' (MockFSZipper _ []) = Nothing

findFile' :: Name -> Directory -> ([File], [File])
findFile' name dir = break (\file -> _fileName file == name) (files dir)

findFile :: Name -> Directory -> Maybe FileType
findFile name dir = _fileType <$> find (\file -> _fileName file == name) (files dir)

findDir' :: Name -> Directory -> ([Directory], [Directory])
findDir' name dir = break (\d -> dirName d == name) (subdirs dir)

findDir :: Name -> Directory -> Maybe Directory
findDir name dir = find (\d -> dirName d == name) (subdirs dir)

isValidName :: Name -> Bool
isValidName name = isValid name && notElem '/' name

cd :: FilePath -> MockFSZipper -> Either IOError MockFSZipper
cd p z =
  let (path, fs) =
        if isAbsolute p
           then (drop 1 p, root z)
           else (p, z)
  in foldlM cd' fs (splitDirectories path)
  where
    cd' :: MockFSZipper -> Name -> Either IOError MockFSZipper
    cd' zipper "." = Right zipper
    cd' zipper ".." = return $ up zipper
    cd' (MockFSZipper dir bs) name =
      case findDir' name dir of
        (ls,subdir:rs) ->
          Right $ MockFSZipper subdir (MockFSCrumb (dirNode dir) ls rs:bs)
        (_,[]) ->
          maybe (Left $ mkIOError NoSuchThing "Mock.Internal.cd" Nothing (Just p))
                (const $ Left $ mkIOError InappropriateType "Mock.Internal.cd" Nothing (Just p))
                (findFile name dir)

mkdir :: Name -> MockFSZipper -> Either IOError MockFSZipper
mkdir name (MockFSZipper cwd bs) =
  if isJust $ findFile name cwd
    then Left alreadyExists
    else
      case findDir' name cwd of
        (_, []) ->
          if isValidName name
            then
              let newDir = directory name [] []
              in
                Right $ MockFSZipper (directory (dirName cwd) (files cwd) (newDir:subdirs cwd))
                                     bs
            else Left $ mkIOError InvalidArgument "Mock.Internal.mkdir" Nothing (Just name)
        _ -> Left alreadyExists
  where
    alreadyExists :: IOError
    alreadyExists = mkIOError AlreadyExists "Mock.Internal.mkdir" Nothing (Just name)

mkfile :: Name -> FileType -> Bool -> MockFSZipper -> Either IOError MockFSZipper
mkfile name filetype clobber (MockFSZipper cwd bs) =
  case findFile' name cwd of
    (ls, _:rs) ->
      if clobber
         then mkfile' $ ls ++ rs
         else Left alreadyExists
    _ ->
      maybe (mkfile' $ files cwd)
            (const $ Left alreadyExists)
            (findDir name cwd)
  where
    mkfile' :: [File] -> Either IOError MockFSZipper
    mkfile' fs =
      if isValidName name
        then
          let newFile = File name filetype
          in
            Right $ MockFSZipper (directory (dirName cwd) (newFile:fs) (subdirs cwd))
                                 bs
        else Left $ mkIOError InvalidArgument "Mock.Internal.mkfile" Nothing (Just name)
    alreadyExists :: IOError
    alreadyExists = mkIOError AlreadyExists "Mock.Internal.mkfile" Nothing (Just name)

rmfile :: Name -> MockFSZipper -> Either IOError MockFSZipper
rmfile name (MockFSZipper cwd bs) =
  if isJust $ findDir name cwd
     then Left $ mkIOError InappropriateType "Mock.Internal.rmfile" Nothing (Just name)
     else
       case findFile' name cwd of
         (ls, _:rs) -> Right $ MockFSZipper (directory (dirName cwd) (ls ++ rs) (subdirs cwd))
                                            bs
         _ -> Left $ mkIOError NoSuchThing "Mock.Internal.rmdir" Nothing (Just name)

-- Note: recursive!
rmdir :: Name -> MockFSZipper -> Either IOError MockFSZipper
rmdir name (MockFSZipper cwd bs) =
  if isJust $ findFile name cwd
     then Left $ mkIOError InappropriateType "Mock.Internal.rmdir" Nothing (Just name)
     else
       case findDir' name cwd of
         (ls, _:rs) -> Right $ MockFSZipper (directory (dirName cwd) (files cwd) (ls ++ rs))
                                            bs
         _ -> Left $ mkIOError NoSuchThing "Mock.Internal.rmdir" Nothing (Just name)
