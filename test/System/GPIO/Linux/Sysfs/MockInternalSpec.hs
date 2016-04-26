{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.MockInternalSpec (spec) where

import GHC.IO.Exception (IOErrorType(..))
import System.GPIO.Linux.Sysfs.Mock.Internal
import System.IO.Error (ioeGetErrorType, isAlreadyExistsError, isDoesNotExistError)
import Test.Hspec

isInvalidArgumentErrorType :: IOErrorType -> Bool
isInvalidArgumentErrorType InvalidArgument = True
isInvalidArgumentErrorType _ = False

isInvalidArgumentError :: IOError -> Bool
isInvalidArgumentError = isInvalidArgumentErrorType . ioeGetErrorType

isInappropriateTypeErrorType :: IOErrorType -> Bool
isInappropriateTypeErrorType InappropriateType = True
isInappropriateTypeErrorType _ = False

isInappropriateTypeError :: IOError -> Bool
isInappropriateTypeError = isInappropriateTypeErrorType . ioeGetErrorType

sysfsRoot :: Directory
sysfsRoot =
  directory "/"
            []
            [directory "sys"
                       []
                       [directory "class"
                                  []
                                  [directory "gpio"
                                             [File "export" Export
                                             ,File "unexport" Unexport]
                                             []]]]

parentName :: MockFSCrumb -> Name
parentName = _dirNodeName . _node

spec :: Spec
spec =
  let sysfsRootZ = MockFSZipper sysfsRoot []
  in
    do describe "cd" $ do

         context "relative paths" $ do

           it "can traverse downwards one directory at a time" $
             do let Right z1@(MockFSZipper dir1 (crumb1:_)) = cd "sys" sysfsRootZ
                dirName dir1 `shouldBe` "sys"
                parentName crumb1 `shouldBe` "/"
                let Right z2@(MockFSZipper dir2 (crumb2:_)) = cd "class" z1
                dirName dir2 `shouldBe` "class"
                parentName crumb2 `shouldBe` "sys"
                let Right (MockFSZipper dir3 (crumb3:_)) = cd "gpio" z2
                dirName dir3 `shouldBe` "gpio"
                parentName crumb3 `shouldBe` "class"

           it "can traverse downwards multiple directories at a time" $
             do let Right (MockFSZipper dir1 (crumb1:_)) = cd "sys/class/gpio" sysfsRootZ
                dirName dir1 `shouldBe` "gpio"
                parentName crumb1 `shouldBe` "class"

           it "fails when changing to a non-existent child" $
             do let Left result1 = cd "foobar" sysfsRootZ
                isDoesNotExistError result1 `shouldBe` True
                let Left result2 = (cd "sys/class" sysfsRootZ >>= cd "baz" )
                isDoesNotExistError result2 `shouldBe` True

           it "fails when changing to a non-existent grandchild" $
             do let Left result = cd "sys/class/foobar" sysfsRootZ
                isDoesNotExistError result `shouldBe` True

           it "fails when changing to a file name rather than a directory name" $
             do let Left result1 = (cd "sys/class/gpio" sysfsRootZ >>= cd "export")
                isInappropriateTypeError result1 `shouldBe` True
                let Left result2 = (cd "sys/class/gpio" sysfsRootZ >>= cd "export/foobar")
                isInappropriateTypeError result2 `shouldBe` True
                let Left result3 = cd "sys/class/gpio/unexport" sysfsRootZ
                isInappropriateTypeError result3 `shouldBe` True
                let Left result4 = cd "sys/class/gpio/unexport/baz" sysfsRootZ
                isInappropriateTypeError result4 `shouldBe` True

           it "'.' in paths" $
             do cd "sys/." sysfsRootZ `shouldBe` cd "sys" sysfsRootZ
                cd "sys/./class" sysfsRootZ `shouldBe` cd "sys/class" sysfsRootZ

           it "'..' in paths" $
             do cd "sys/.." sysfsRootZ `shouldBe` Right sysfsRootZ
                cd "sys/class/../class" sysfsRootZ `shouldBe` cd "sys/class" sysfsRootZ
                cd "sys/class/gpio/../../class" sysfsRootZ `shouldBe` cd "sys/../sys/class/../class/gpio/.." sysfsRootZ
                cd "sys/class/gpio/../../.." sysfsRootZ `shouldBe` Right sysfsRootZ

           it "'..' beyond root clamps to root" $
             do cd "sys/class/../../../.." sysfsRootZ `shouldBe` Right sysfsRootZ
                cd "../.." sysfsRootZ `shouldBe` Right sysfsRootZ

         context "absolute paths" $ do
           it "can traverse downwards one directory at a time" $
             do let Right z1@(MockFSZipper dir1 (crumb1:_)) = cd "/sys" sysfsRootZ
                dirName dir1 `shouldBe` "sys"
                parentName crumb1 `shouldBe` "/"
                let Right z2@(MockFSZipper dir2 (crumb2:_)) = cd "class" z1
                dirName dir2 `shouldBe` "class"
                parentName crumb2 `shouldBe` "sys"
                let Right (MockFSZipper dir3 (crumb3:_)) = cd "gpio" z2
                dirName dir3 `shouldBe` "gpio"
                parentName crumb3 `shouldBe` "class"
           it "can traverse downwards multiple directories at a time" $
             do let Right (MockFSZipper dir1 (crumb1:_)) = cd "/sys/class/gpio" sysfsRootZ
                dirName dir1 `shouldBe` "gpio"
                parentName crumb1 `shouldBe` "class"
           it "fails when changing to a non-existent child" $
             do let Left result = cd "/foobar" sysfsRootZ
                isDoesNotExistError result `shouldBe` True
           it "fails when changing to a non-existent grandchild" $
             do let Left result = cd "/sys/class/foobar" sysfsRootZ
                isDoesNotExistError result `shouldBe` True
           it "fails when changing to a file name rather than a directory name" $
             do let Left result1 = cd "/sys/class/gpio/export" sysfsRootZ
                isInappropriateTypeError result1 `shouldBe` True
                let Left result2 = cd "/sys/class/gpio/unexport/baz" sysfsRootZ
                isInappropriateTypeError result2 `shouldBe` True

           it "cd / is root" $
             cd "/" sysfsRootZ `shouldBe` Right sysfsRootZ

           it "cd / from deeper starts at root" $ do
             (cd "/sys/class/gpio" sysfsRootZ >>= cd "/") `shouldBe` Right sysfsRootZ
             (cd "/sys/class/gpio" sysfsRootZ >>= cd "/sys/class") `shouldBe` cd "/sys/class" sysfsRootZ

           it "'.' in paths" $
             do cd "/sys/." sysfsRootZ `shouldBe` cd "/sys" sysfsRootZ
                cd "/sys/./class" sysfsRootZ `shouldBe` cd "/sys/class" sysfsRootZ
           it "'..' in paths" $
             do cd "/sys/.." sysfsRootZ `shouldBe` Right sysfsRootZ
                cd "/sys/class/../class" sysfsRootZ `shouldBe` cd "/sys/class" sysfsRootZ
                cd "/sys/class/gpio/../../class" sysfsRootZ `shouldBe` cd "/sys/../sys/class/../class/gpio/.." sysfsRootZ
                cd "/sys/class/gpio/../../.." sysfsRootZ `shouldBe` Right sysfsRootZ
           it "'..' beyond root clamps to root" $
             do cd "/sys/class/../../../.." sysfsRootZ `shouldBe` Right sysfsRootZ
                cd "/../.." sysfsRootZ `shouldBe` Right sysfsRootZ

         context "absolute and relative paths" $ do
           it "produce the same result when they lead to the same directory" $
             do cd "sys/class/gpio" sysfsRootZ `shouldBe` cd "/sys/class/gpio" sysfsRootZ
                cd "sys/class/gpio/.." sysfsRootZ `shouldBe` cd "/sys/class/gpio/.." sysfsRootZ
                cd "sys/class/gpio/../../." sysfsRootZ `shouldBe` cd "/sys/class/gpio/../../." sysfsRootZ
                cd "sys/class/.././class/../../sys" sysfsRootZ `shouldBe` cd "/sys/../sys/class/./gpio/../.." sysfsRootZ

       describe "mkdir" $
         do it "creates a subdirectory in the current directory" $
              do let Right z1@(MockFSZipper dir1 crumb1) = mkdir "xyzzy" sysfsRootZ
                 dirName dir1 `shouldBe` "/"
                 crumb1 `shouldBe` []
                 let Right (MockFSZipper dir2 (crumb2:_)) = cd "xyzzy" z1
                 dirName dir2 `shouldBe` "xyzzy"
                 parentName crumb2 `shouldBe` "/"

            it "can create multiple subdirectories in the same directory" $
              do let Right z1@(MockFSZipper dir1 crumb1) = mkdir "xyzzy" sysfsRootZ
                 dirName dir1 `shouldBe` "/"
                 crumb1 `shouldBe` []
                 let Right z2@(MockFSZipper dir2 crumb2) = mkdir "plugh" z1
                 dirName dir2 `shouldBe` "/"
                 crumb2 `shouldBe` []
                 let Right z3@(MockFSZipper dir3 (crumb3:_)) = cd "xyzzy" z2
                 dirName dir3 `shouldBe` "xyzzy"
                 parentName crumb3 `shouldBe` "/"
                 let Right (MockFSZipper dir4 (crumb4:_)) = cd "../plugh" z3
                 dirName dir4 `shouldBe` "plugh"
                 parentName crumb4 `shouldBe` "/"

            it "works when nested" $
              do let Right (MockFSZipper dir (crumb:_)) = mkdir "abc" sysfsRootZ >>= cd "/abc" >>= mkdir "def" >>= cd "/abc/def"
                 dirName dir `shouldBe` "def"
                 parentName crumb `shouldBe` "abc"

            it "fails when a subdir with the same name already exists" $
              do let Left result = mkdir "sys" sysfsRootZ
                 isAlreadyExistsError result `shouldBe` True

            it "fails when a file with the same name already exists" $
              do let Left result = (cd "/sys/class/gpio" sysfsRootZ >>= mkdir "export")
                 isAlreadyExistsError result `shouldBe` True

            it "fails with an invalid name" $
              do let Left result = mkdir "" sysfsRootZ
                 isInvalidArgumentError result `shouldBe` True

            it "fails when the name contains a '/'" $
              do let Left result1 = mkdir "/abc" sysfsRootZ
                 isInvalidArgumentError result1 `shouldBe` True
                 let Left result2 = mkdir "sys/foobar" sysfsRootZ
                 isInvalidArgumentError result2 `shouldBe` True

       describe "mkfile" $
         do it "creates a file in the current directory when clobber is False" $
              do let Right z1 = cd "/sys/class/gpio" sysfsRootZ
                 let Right (MockFSZipper dir2 (crumb2:_)) = mkfile "gpio1" (Const ["Hey!", "This is gpio1"]) False z1
                 dirName dir2 `shouldBe` "gpio"
                 parentName crumb2 `shouldBe` "class"
                 let file:rest = files dir2
                 _fileName file `shouldBe` "gpio1"
                 _fileType file `shouldBe` (Const ["Hey!", "This is gpio1"])
                 rest `shouldBe` [File {_fileName = "export", _fileType = Export},File {_fileName = "unexport", _fileType = Unexport}]

            it "creates a file in the current directory when clobber is True" $
              do let Right z1 = cd "/sys/class/gpio" sysfsRootZ
                 let Right (MockFSZipper dir2 (crumb2:_)) = mkfile "gpio1" (Const ["Hey!", "This is gpio1"]) True z1
                 dirName dir2 `shouldBe` "gpio"
                 parentName crumb2 `shouldBe` "class"
                 let file:rest = files dir2
                 _fileName file `shouldBe` "gpio1"
                 _fileType file `shouldBe` (Const ["Hey!", "This is gpio1"])
                 rest `shouldBe` [File {_fileName = "export", _fileType = Export},File {_fileName = "unexport", _fileType = Unexport}]

            it "fails when a subdir with the same name already exists" $ do
              let Left result1 = mkfile "sys" (Const []) True sysfsRootZ
              isAlreadyExistsError result1 `shouldBe` True
              let Left result2 = mkfile "sys" (Const []) False sysfsRootZ
              isAlreadyExistsError result2 `shouldBe` True

            it "fails when a file with the same name already exists and clobber is False" $
              do let Left result = (cd "/sys/class/gpio" sysfsRootZ >>= mkfile "export" (Const []) False)
                 isAlreadyExistsError result `shouldBe` True

            it "overwrites an existing file's contents when a file with the same name already exists and clobber is True" $
              do let Right z1 = cd "/sys/class/gpio" sysfsRootZ
                 let Right z2 = mkfile "gpio1" (Const ["Hey!", "This is gpio1"]) False z1
                 let Right (MockFSZipper dir3 (crumb3:_)) = mkfile "gpio1" (Const ["Hey!", "Now I'm gpio1"]) True z2
                 dirName dir3 `shouldBe` "gpio"
                 parentName crumb3 `shouldBe` "class"
                 let file:rest = files dir3
                 _fileName file `shouldBe` "gpio1"
                 _fileType file `shouldBe` (Const ["Hey!", "Now I'm gpio1"])
                 rest `shouldBe` [File {_fileName = "export", _fileType = Export},File {_fileName = "unexport", _fileType = Unexport}]

            it "fails with an invalid name" $ do
              let Left result1 = mkfile "" (Const []) False sysfsRootZ
              isInvalidArgumentError result1 `shouldBe` True
              let Left result2 = mkfile "" (Const []) True sysfsRootZ
              isInvalidArgumentError result2 `shouldBe` True

            it "fails when the name contains a '/'" $
              do let Left result1 = mkfile "/abc" (Const []) False sysfsRootZ
                 isInvalidArgumentError result1 `shouldBe` True
                 let Left result2 = mkfile "/abc" (Const []) True sysfsRootZ
                 isInvalidArgumentError result2 `shouldBe` True
                 let Left result3 = mkfile "sys/foobar" (Const []) False sysfsRootZ
                 isInvalidArgumentError result3 `shouldBe` True
                 let Left result4 = mkfile "sys/foobar" (Const []) True sysfsRootZ
                 isInvalidArgumentError result4 `shouldBe` True

       describe "findFile" $
         do it "finds files in the current directory" $
              do let Right z1@(MockFSZipper dir1 _) = cd "/sys/class/gpio" sysfsRootZ
                 findFile "export" dir1 `shouldBe` (Just Export)
                 findFile "unexport" dir1 `shouldBe` (Just Unexport)
                 let Right (MockFSZipper dir2 _) = mkfile "gpio1" (Const ["Hey!", "This is gpio1"]) False z1
                 findFile "gpio1" dir2 `shouldBe` (Just ((Const ["Hey!", "This is gpio1"])))
            it "doesn't find subdirectories in the current directory" $
              do let Right (MockFSZipper dir1 _) = cd "/sys/class" sysfsRootZ
                 findFile "gpio" dir1 `shouldBe` Nothing
            it "returns failure on non-existent files" $
              do let Right (MockFSZipper dir1 _) = cd "/sys/class" sysfsRootZ
                 findFile "foobar" dir1 `shouldBe` Nothing
                 findFile "export" dir1 `shouldBe` Nothing
            it "doesn't find files in subdirectories of the current directory" $
              do let Right (MockFSZipper dir1 _) = cd "/sys/class" sysfsRootZ
                 findFile "gpio/export" dir1 `shouldBe` Nothing

       describe "rmdir" $
         do it "removes a subdirectory of the current directory" $
              do let Right z1 = cd "/sys" sysfsRootZ >>= mkdir "xyzzy" >>= mkdir "plugh"
                 let Right z2@(MockFSZipper dir2 (crumb2:_)) = rmdir "xyzzy" z1
                 dirName dir2 `shouldBe` "sys"
                 parentName crumb2 `shouldBe` "/"
                 let Right (MockFSZipper dir3 (crumb3:_)) = rmdir "plugh" z2
                 dirName dir3 `shouldBe` "sys"
                 parentName crumb3 `shouldBe` "/"

            it "fails when no subdir with the name exists" $
              do let Left result = rmdir "foo" sysfsRootZ
                 isDoesNotExistError result `shouldBe` True

            it "fails when a file is named" $
              do let Left result = (cd "/sys/class/gpio" sysfsRootZ >>= rmdir "export")
                 isInappropriateTypeError result `shouldBe` True

            it "fails when the name contains a '/'" $
              do let Left result1 = rmdir "/sys" sysfsRootZ
                 isDoesNotExistError result1 `shouldBe` True
                 let Left result2 = rmdir "sys/class" sysfsRootZ
                 isDoesNotExistError result2 `shouldBe` True

       describe "rmfile" $
         do it "removes a file in the current directory" $
              do let Right z1 = cd "/sys" sysfsRootZ >>= mkfile "abc" (Const []) False >>= mkfile "def" (Const []) False
                 let Right z2@(MockFSZipper dir2 (crumb2:_)) = rmfile "abc" z1
                 dirName dir2 `shouldBe` "sys"
                 parentName crumb2 `shouldBe` "/"
                 files dir2 `shouldBe` [File {_fileName = "def", _fileType = (Const [])}]
                 let Right (MockFSZipper dir3 (crumb3:_)) = rmfile "def" z2
                 dirName dir3 `shouldBe` "sys"
                 parentName crumb3 `shouldBe` "/"
                 files dir3 `shouldBe` []

            it "fails when no file with the name exists" $
              do let Left result = rmfile "foo" sysfsRootZ
                 isDoesNotExistError result `shouldBe` True

            it "fails when a directory is named" $
              do let Left result = (cd "/sys/class" sysfsRootZ >>= rmfile "gpio")
                 isInappropriateTypeError result `shouldBe` True

            it "fails when the name contains a '/'" $
              do let Left result = rmfile "/sys/class/gpio/export" sysfsRootZ
                 isDoesNotExistError result `shouldBe` True

       describe "pathFromRoot" $
         do it "returns the path from the current directory to the root directory" $
              do pathFromRoot sysfsRootZ `shouldBe` "/"
                 let Right z1 = cd "/sys/class/gpio" sysfsRootZ
                 pathFromRoot z1 `shouldBe` "/sys/class/gpio"
                 let Right z2 = cd "/sys/class/gpio/../../class" sysfsRootZ
                 pathFromRoot z2 `shouldBe` "/sys/class"
