{-|
Module      : System.GPIO.Linux.Sysfs.Mock.Internal
Description : Functions used by the mock MonadSysfs instance.
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Types and functions to emulate a (pure) rudimentary Posix-style
filesystem.

This module is exported in order to test it, but you should not rely
on its contents.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module System.GPIO.Linux.Sysfs.Mock.Internal
       ( -- * Mock filesystem types
         Name
       , File(..)
       , DirNode(..)
       , Directory
       , directory
       , dirName
       , files
       , root
       , subdirs
       , MockFSException(..)
       , MockFSCrumb(..)
       , MockFSZipper
         -- * Mock filesystem operations
       , up
       , cd
       , cwd
       , top
       , pathFromRoot
       , findFile
       , findFile'
       , findDir
       , findDir'
       , mkdir
       , mkfile
       , rmdir
       , rmfile
       ) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Foldable (foldlM)
import Data.List (find, unfoldr)
import Data.Maybe (isJust)
import Data.Tree (Tree(..))
import Data.Typeable (Typeable)
import System.FilePath (isAbsolute, isValid, joinPath, splitDirectories)

type Name = String

data File =
  File {_fileName :: Name
       ,_contents :: [ByteString]}
  deriving (Show,Eq)

data DirNode =
  DirNode {_dirName :: Name
          ,_files :: [File]}
  deriving (Show,Eq)

type Directory = Tree DirNode

-- Lenses and prisms.

directory :: Name -> [File] -> [Directory] -> Directory
directory name fs subs = Node (DirNode name fs) subs

dirName :: Directory -> Name
dirName = _dirName . root

files :: Directory -> [File]
files = _files . root

root :: Directory -> DirNode
root = rootLabel

subdirs :: Directory -> [Directory]
subdirs = subForest

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
  MockFSCrumb {_root :: DirNode
              ,_pred :: [Directory]
              ,_succ :: [Directory]}
  deriving (Show,Eq)

type MockFSZipper = (Directory, [MockFSCrumb])

-- Logically equivalent to "cd .."
up :: MockFSZipper -> MockFSZipper
up (dir, MockFSCrumb parent ls rs:bs) = (directory (_dirName parent) (_files parent) (ls ++ [dir] ++ rs), bs)
up (dir, []) = (dir, []) -- cd /.. == /

top :: MockFSZipper -> MockFSZipper
top (t, []) = (t, [])
top z = top $ up z

pathFromRoot :: MockFSZipper -> FilePath
pathFromRoot zipper =
  joinPath $ "/" : reverse (unfoldr walkUp zipper)
  where
    walkUp :: MockFSZipper -> Maybe (Name, MockFSZipper)
    walkUp z@(dir, _:_) = Just (dirName dir, up z)
    walkUp (_, []) = Nothing

cwd :: MockFSZipper -> Directory
cwd (dir, _) = dir

findFile :: Name -> Directory -> ([File], [File])
findFile name dir = break (\file -> _fileName file == name) (files dir)

findFile' :: Name -> Directory -> Maybe File
findFile' name dir = find (\file -> _fileName file == name) (files dir)

findDir :: Name -> Directory -> ([Directory], [Directory])
findDir name dir = break (\d -> dirName d == name) (subdirs dir)

findDir' :: Name -> Directory -> Maybe Directory
findDir' name dir = find (\d -> dirName d == name) (subdirs dir)

isValidName :: Name -> Bool
isValidName name = isValid name && notElem '/' name

cd :: FilePath -> MockFSZipper -> Either MockFSException MockFSZipper
cd p z =
  let (path, fs) =
        if isAbsolute p
           then (drop 1 p, top z)
           else (p, z)
  in foldlM cd' fs (splitDirectories path)
  where
    cd' :: MockFSZipper -> Name -> Either MockFSException MockFSZipper
    cd' zipper "." = Right zipper
    cd' zipper ".." = return $ up zipper
    cd' (dir,bs) name =
      case findDir name dir of
        (ls,subdir:rs) ->
          Right (subdir
                ,MockFSCrumb (root dir) ls rs:bs)
        (_,[]) ->
          maybe (Left $ NoSuchFileOrDirectory p)
                (const $ Left $ NotADirectory p)
                (findFile' name dir)

mkdir :: Name -> MockFSZipper -> Either MockFSException MockFSZipper
mkdir name (parent, bs) =
  if isJust $ findFile' name parent
    then Left $ FileExists name
    else
      case findDir name parent of
        (_, []) ->
          if isValidName name
            then
              let child = directory name [] []
              in
                Right (directory (dirName parent) (files parent) (child:subdirs parent), bs)
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
      maybe (mkfile' $ files parent)
            (const $ Left (FileExists name))
            (findDir' name parent)
  where
    mkfile' :: [File] -> Either MockFSException MockFSZipper
    mkfile' fs =
      if isValidName name
        then
          let file = File name contents
          in
            Right (directory (dirName parent) (file:fs) (subdirs parent), bs)
        else Left $ InvalidName name

rmfile :: Name -> MockFSZipper -> Either MockFSException MockFSZipper
rmfile name (parent, bs) =
  if isJust $ findDir' name parent
     then Left $ NotAFile name
     else
       case findFile name parent of
         (ls, _:rs) -> Right (directory (dirName parent) (ls ++ rs) (subdirs parent), bs)
         _ -> Left $ NoSuchFileOrDirectory name

-- Note: recursive!
rmdir :: Name -> MockFSZipper -> Either MockFSException MockFSZipper
rmdir name (parent, bs) =
  if isJust $ findFile' name parent
     then Left $ NotADirectory name
     else
       case findDir name parent of
         (ls, _:rs) -> Right (directory (dirName parent) (files parent) (ls ++ rs), bs)
         _ -> Left $ NoSuchFileOrDirectory name
