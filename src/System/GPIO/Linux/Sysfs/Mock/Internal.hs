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
       , Directory(..)
       , MockFSException(..)
       , MockFSCrumb(..)
       , MockFSZipper
         -- * Mock filesystem operations
       , up
       , cd
       , cwd
       , root
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
import Data.List (break, delete, find, reverse, unfoldr)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import System.FilePath (isAbsolute, isValid, joinPath, splitDirectories)

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
up (dir, MockFSCrumb parent files ls rs:bs) = (Directory parent files (ls ++ [dir] ++ rs), bs)
up (dir, []) = (dir, []) -- cd /.. == /

root :: MockFSZipper -> MockFSZipper
root (top, []) = (top, [])
root z = root $ up z

pathFromRoot :: MockFSZipper -> FilePath
pathFromRoot zipper =
  joinPath $ ["/"] ++ reverse (unfoldr walkUp zipper)
  where
    walkUp :: MockFSZipper -> Maybe (Name, MockFSZipper)
    walkUp z@(dir, _:_) = Just (_dirName dir, up z)
    walkUp (_, []) = Nothing

cwd :: MockFSZipper -> Directory
cwd (dir, _) = dir

findFile :: Name -> Directory -> ([File], [File])
findFile name dir = break (\file -> _fileName file == name) (_files dir)

findFile' :: Name -> Directory -> Maybe File
findFile' name dir = find (\file -> _fileName file == name) (_files dir)

findDir :: Name -> Directory -> ([Directory], [Directory])
findDir name dir = break (\d -> _dirName d == name) (_subdirs dir)

findDir' :: Name -> Directory -> Maybe Directory
findDir' name dir = find (\d -> _dirName d == name) (_subdirs dir)

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
        cd' zipper@(dir,bs) name =
          case name of
            "." -> Right zipper
            ".." -> return $ up zipper
            _ ->
              case findDir name dir of
                (ls,subdir:rs) ->
                  Right (subdir
                        ,MockFSCrumb (_dirName dir)
                                     (_files dir)
                                     ls
                                     rs :
                         bs)
                (_,[]) ->
                  maybe (Left $ NoSuchFileOrDirectory p)
                        (const $ Left $ NotADirectory p)
                        (findFile' name dir)


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
