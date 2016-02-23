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
       , root
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
import Data.List (break, delete, find)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import System.FilePath (isAbsolute, isValid, splitDirectories)

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
