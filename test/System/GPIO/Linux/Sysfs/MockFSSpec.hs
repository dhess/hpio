{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.MockFSSpec (spec) where

import System.GPIO.Linux.Sysfs.Mock
import Test.Hspec

spec :: Spec
spec =
  let sysfsRootZ = (sysfsRoot, [])
  in
    do describe "cd" $ do

         context "relative paths" $ do

           it "can traverse downwards one directory at a time" $
             do let Right z1@(dir1, crumb1:_) = cd "sys" sysfsRootZ
                _dirName dir1 `shouldBe` "sys"
                _parentName crumb1 `shouldBe` "/"
                let Right z2@(dir2, crumb2:_) = cd "class" z1
                _dirName dir2 `shouldBe` "class"
                _parentName crumb2 `shouldBe` "sys"
                let Right (dir3, crumb3:_) = cd "gpio" z2
                _dirName dir3 `shouldBe` "gpio"
                _parentName crumb3 `shouldBe` "class"

           it "can traverse downwards multiple directories at a time" $
             do let Right (dir1, crumb1:_) = cd "sys/class/gpio" sysfsRootZ
                _dirName dir1 `shouldBe` "gpio"
                _parentName crumb1 `shouldBe` "class"

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
             do let Right z1@(dir1, crumb1:_) = cd "/sys" sysfsRootZ
                _dirName dir1 `shouldBe` "sys"
                _parentName crumb1 `shouldBe` "/"
                let Right z2@(dir2, crumb2:_) = cd "class" z1
                _dirName dir2 `shouldBe` "class"
                _parentName crumb2 `shouldBe` "sys"
                let Right (dir3, crumb3:_) = cd "gpio" z2
                _dirName dir3 `shouldBe` "gpio"
                _parentName crumb3 `shouldBe` "class"
           it "can traverse downwards multiple directories at a time" $
             do let Right (dir1, crumb1:_) = cd "/sys/class/gpio" sysfsRootZ
                _dirName dir1 `shouldBe` "gpio"
                _parentName crumb1 `shouldBe` "class"
           it "fails when changing to a non-existent child" $
             do cd "/foobar" sysfsRootZ `shouldBe` (Left $ NoSuchFileOrDirectory "/foobar")
           it "fails when changing to a non-existent grandchild" $
             cd "/sys/class/foobar" sysfsRootZ `shouldBe` (Left $ NoSuchFileOrDirectory "/sys/class/foobar")
           it "fails when changing to a file name rather than a directory name" $
             do cd "/sys/class/gpio/export" sysfsRootZ `shouldBe` (Left $ NotADirectory "/sys/class/gpio/export")
                cd "/sys/class/gpio/unexport/baz" sysfsRootZ `shouldBe` (Left $ NotADirectory "/sys/class/gpio/unexport/baz")

           it "cd / is root" $
             cd "/" sysfsRootZ `shouldBe` Right sysfsRootZ

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
