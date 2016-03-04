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

This module was written for internal use only. Its interface may
change at any time. Documentation in this module is sparse, by design.

N.B.: This mock filesystem implementation was written with the
intention of doing only just enough to emulate the operations needed
by the 'System.GPIO.Linux.Sysfs.Monad.MonadSysfs' type class. Though
it may be possible to use this implementation for other purposes, it
has neither been designed nor tested for that. Use at your own risk
and please do not submit requests for addtional functionality.

-}

{-# LANGUAGE DeriveDataTypeable #-}
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
       , MockFSException(..)
       , MockFSCrumb(..)
       , MockFSZipper(..)
         -- * Mock filesystem operations
       , up
       , cd
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
import Data.List (find, unfoldr)
import Data.Maybe (isJust)
import Data.Tree (Tree(..))
import Data.Typeable (Typeable)
import System.FilePath (isAbsolute, isValid, joinPath, splitDirectories)
import System.GPIO.Types (Pin)

type Name = String

data FileType
  = Const [ByteString]
  | Export
  | Unexport
  | Value Pin
  | Direction Pin
  | Edge Pin
  | ActiveLow Pin
  deriving (Show,Eq)

data File =
  File {_fileName :: Name
       ,_fileType :: FileType}
  deriving (Show,Eq)

data DirNode =
  DirNode {_dirName :: Name
          ,_files :: [File]}
  deriving (Show,Eq)

type Directory = Tree DirNode

-- Getters.

directory :: Name -> [File] -> [Directory] -> Directory
directory name fs subs = Node (DirNode name fs) subs

dirName :: Directory -> Name
dirName = _dirName . dirNode

files :: Directory -> [File]
files = _files . dirNode

dirNode :: Directory -> DirNode
dirNode = rootLabel

subdirs :: Directory -> [Directory]
subdirs = subForest

-- | Exceptions that can be thrown by mock @sysfs@ filesystem
-- operations.
--
-- Generally speaking these exceptions do not map directly to their
-- 'IO' equivalents, since those are usually a bit too low-level to be
-- useful at a macro level (e.g., "why did I get an invalid operation
-- exception when trying to write that GPIO pin?"); rather, these
-- exceptions attempt to be descriptive in the context of the
-- higher-level operation you were trying to perform.
data MockFSException
  = ReadOnlyFile FilePath          -- ^ Attempt to write a read-only file
  | WriteOnlyFile FilePath         -- ^ Attempt to read a write-only file
  | WriteError FilePath            -- ^ Data written is not formatted properly
  | NotADirectory FilePath         -- ^ Path specifies a file, not a directory
  | NotAFile FilePath              -- ^ Path specifies a directory, not a file
  | NoSuchFileOrDirectory FilePath -- ^ File/directory does not exist
  | FileExists Name                -- ^ Attempt to create a file which already exists
  | InvalidName Name               -- ^ Invalid file/path name
  | PinAlreadyExists Pin           -- ^ Mock state is invalid because the same pin occurs more than once
  | InvalidPin Pin                 -- ^ Attempt to perform an operation on a pin which doesn't exist
  | AlreadyExported Pin            -- ^ Attempt to export a pin that's already been exported
  | NotExported Pin                -- ^ Attempt to unexport a pin that isn't exported
  | InternalError String           -- ^ A condition has occurred in the implementation which should never occur
  deriving (Show,Eq,Typeable)

instance Exception MockFSException

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
  MockFSZipper (directory (_dirName parent) (_files parent) (ls ++ [dir] ++ rs))
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

findFile :: Name -> Directory -> ([File], [File])
findFile name dir = break (\file -> _fileName file == name) (files dir)

findFile' :: Name -> Directory -> Maybe FileType
findFile' name dir = _fileType <$> find (\file -> _fileName file == name) (files dir)

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
           then (drop 1 p, root z)
           else (p, z)
  in foldlM cd' fs (splitDirectories path)
  where
    cd' :: MockFSZipper -> Name -> Either MockFSException MockFSZipper
    cd' zipper "." = Right zipper
    cd' zipper ".." = return $ up zipper
    cd' (MockFSZipper dir bs) name =
      case findDir name dir of
        (ls,subdir:rs) ->
          Right $ MockFSZipper subdir (MockFSCrumb (dirNode dir) ls rs:bs)
        (_,[]) ->
          maybe (Left $ NoSuchFileOrDirectory p)
                (const $ Left $ NotADirectory p)
                (findFile' name dir)

mkdir :: Name -> MockFSZipper -> Either MockFSException MockFSZipper
mkdir name (MockFSZipper parent bs) =
  if isJust $ findFile' name parent
    then Left $ FileExists name
    else
      case findDir name parent of
        (_, []) ->
          if isValidName name
            then
              let child = directory name [] []
              in
                Right $ MockFSZipper (directory (dirName parent) (files parent) (child:subdirs parent))
                                     bs
            else Left $ InvalidName name
        _ -> Left $ FileExists name

mkfile :: Name -> FileType -> Bool -> MockFSZipper -> Either MockFSException MockFSZipper
mkfile name filetype clobber (MockFSZipper parent bs) =
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
          let file = File name filetype
          in
            Right $ MockFSZipper (directory (dirName parent) (file:fs) (subdirs parent))
                                 bs
        else Left $ InvalidName name

rmfile :: Name -> MockFSZipper -> Either MockFSException MockFSZipper
rmfile name (MockFSZipper parent bs) =
  if isJust $ findDir' name parent
     then Left $ NotAFile name
     else
       case findFile name parent of
         (ls, _:rs) -> Right $ MockFSZipper (directory (dirName parent) (ls ++ rs) (subdirs parent))
                                            bs
         _ -> Left $ NoSuchFileOrDirectory name

-- Note: recursive!
rmdir :: Name -> MockFSZipper -> Either MockFSException MockFSZipper
rmdir name (MockFSZipper parent bs) =
  if isJust $ findFile' name parent
     then Left $ NotADirectory name
     else
       case findDir name parent of
         (ls, _:rs) -> Right $ MockFSZipper (directory (dirName parent) (files parent) (ls ++ rs))
                                            bs
         _ -> Left $ NoSuchFileOrDirectory name
