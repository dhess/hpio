{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.MockInternalSpec (spec) where

import System.GPIO.Linux.Sysfs.Mock.Internal
import Test.Hspec

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
             do cd "foobar" sysfsRootZ `shouldBe` (Left $ NoSuchFileOrDirectory "foobar")
                (cd "sys/class" sysfsRootZ >>= cd "baz" ) `shouldBe` (Left $ NoSuchFileOrDirectory "baz")

           it "fails when changing to a non-existent grandchild" $
             cd "sys/class/foobar" sysfsRootZ `shouldBe` (Left $ NoSuchFileOrDirectory "sys/class/foobar")

           it "fails when changing to a file name rather than a directory name" $
             do (cd "sys/class/gpio" sysfsRootZ >>= cd "export") `shouldBe` (Left $ NotADirectory "export")
                (cd "sys/class/gpio" sysfsRootZ >>= cd "export/foobar") `shouldBe` (Left $ NotADirectory "export/foobar")
                cd "sys/class/gpio/unexport" sysfsRootZ `shouldBe` (Left $ NotADirectory "sys/class/gpio/unexport")
                cd "sys/class/gpio/unexport/baz" sysfsRootZ `shouldBe` (Left $ NotADirectory "sys/class/gpio/unexport/baz")

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
             do cd "/foobar" sysfsRootZ `shouldBe` (Left $ NoSuchFileOrDirectory "/foobar")
           it "fails when changing to a non-existent grandchild" $
             cd "/sys/class/foobar" sysfsRootZ `shouldBe` (Left $ NoSuchFileOrDirectory "/sys/class/foobar")
           it "fails when changing to a file name rather than a directory name" $
             do cd "/sys/class/gpio/export" sysfsRootZ `shouldBe` (Left $ NotADirectory "/sys/class/gpio/export")
                cd "/sys/class/gpio/unexport/baz" sysfsRootZ `shouldBe` (Left $ NotADirectory "/sys/class/gpio/unexport/baz")

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
              mkdir "sys" sysfsRootZ `shouldBe` (Left $ FileExists "sys")

            it "fails when a file with the same name already exists" $
              (cd "/sys/class/gpio" sysfsRootZ >>= mkdir "export") `shouldBe` (Left $ FileExists "export")

            it "fails with an invalid name" $
              mkdir "" sysfsRootZ `shouldBe` (Left $ InvalidName "")

            it "fails when the name contains a '/'" $
              do mkdir "/abc" sysfsRootZ `shouldBe` (Left $ InvalidName "/abc")
                 mkdir "sys/foobar" sysfsRootZ `shouldBe` (Left $ InvalidName "sys/foobar")

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
              mkfile "sys" (Const []) True sysfsRootZ `shouldBe` (Left $ FileExists "sys")
              mkfile "sys" (Const []) False sysfsRootZ `shouldBe` (Left $ FileExists "sys")

            it "fails when a file with the same name already exists and clobber is False" $
              (cd "/sys/class/gpio" sysfsRootZ >>= mkfile "export" (Const []) False) `shouldBe` (Left $ FileExists "export")

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
              mkfile "" (Const []) False sysfsRootZ `shouldBe` (Left $ InvalidName "")
              mkfile "" (Const []) True sysfsRootZ `shouldBe` (Left $ InvalidName "")

            it "fails when the name contains a '/'" $
              do mkfile "/abc" (Const []) False sysfsRootZ `shouldBe` (Left $ InvalidName "/abc")
                 mkfile "/abc" (Const []) True sysfsRootZ `shouldBe` (Left $ InvalidName "/abc")
                 mkfile "sys/foobar" (Const []) False sysfsRootZ `shouldBe` (Left $ InvalidName "sys/foobar")
                 mkfile "sys/foobar" (Const []) True sysfsRootZ `shouldBe` (Left $ InvalidName "sys/foobar")

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
              rmdir "foo" sysfsRootZ `shouldBe` (Left $ NoSuchFileOrDirectory "foo")

            it "fails when a file is named" $
              (cd "/sys/class/gpio" sysfsRootZ >>= rmdir "export") `shouldBe` (Left $ NotADirectory "export")

            it "fails when the name contains a '/'" $
              do rmdir "/sys" sysfsRootZ `shouldBe` (Left $ NoSuchFileOrDirectory "/sys")
                 rmdir "sys/class" sysfsRootZ `shouldBe` (Left $ NoSuchFileOrDirectory "sys/class")

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
              rmfile "foo" sysfsRootZ `shouldBe` (Left $ NoSuchFileOrDirectory "foo")

            it "fails when a directory is named" $
              (cd "/sys/class" sysfsRootZ >>= rmfile "gpio") `shouldBe` (Left $ NotAFile "gpio")

            it "fails when the name contains a '/'" $
              rmfile "/sys/class/gpio/export" sysfsRootZ `shouldBe` (Left $ NoSuchFileOrDirectory "/sys/class/gpio/export")

       describe "pathFromRoot" $
         do it "returns the path from the current directory to the root directory" $
              do pathFromRoot sysfsRootZ `shouldBe` "/"
                 let Right z1 = cd "/sys/class/gpio" sysfsRootZ
                 pathFromRoot z1 `shouldBe` "/sys/class/gpio"
                 let Right z2 = cd "/sys/class/gpio/../../class" sysfsRootZ
                 pathFromRoot z2 `shouldBe` "/sys/class"
