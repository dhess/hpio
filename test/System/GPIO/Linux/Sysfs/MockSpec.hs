{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.MockSpec (spec) where

import Prelude hiding (readFile, writeFile)
import Data.List (sort)
import qualified Data.Map.Strict as Map (empty)
import System.GPIO.Linux.Sysfs.Mock
import System.GPIO.Linux.Sysfs.Mock.Internal (Directory, cd, dirName)
import System.GPIO.Types (Pin(..))
import Test.Hspec

cwd :: MockFSZipper -> Directory
cwd (dir, _) = dir

spec :: Spec
spec =
  let defaultMockState = MockState sysfsRootZipper Map.empty
      Right sysz = (cd "/sys") sysfsRootZipper
      syszMockState = MockState sysz Map.empty
  in do
    describe "SysfsMockT" $ do

      context "doesDirectoryExist" $ do
        it "relative paths are relative to the initial zipper's working directory" $ do
          evalSysfsMock (doesDirectoryExist "sys/class/gpio") sysfsRootZipper [] `shouldBe` Right True
          evalSysfsMock (doesDirectoryExist "sys/class/gpio") sysz [] `shouldBe` Right False
          evalSysfsMock (doesDirectoryExist "class/gpio") sysz [] `shouldBe` Right True
        it "absolute paths work regardless of the initial zipper's working directory" $ do
          evalSysfsMock (doesDirectoryExist "/sys/class/gpio") sysfsRootZipper [] `shouldBe` Right True
          evalSysfsMock (doesDirectoryExist "/sys/class/gpio") sysz [] `shouldBe` Right True
        it "doesn't change the initial zipper's state" $ do
          execSysfsMock (doesDirectoryExist "/sys/class/gpio") sysfsRootZipper [] `shouldBe` Right defaultMockState
          execSysfsMock (doesDirectoryExist "/sys/class/gpio") sysz [] `shouldBe` Right syszMockState
        it "returns False on files" $ do
          evalSysfsMock (doesDirectoryExist "/sys/class/gpio/export") sysfsRootZipper [] `shouldBe` Right False
          evalSysfsMock (doesDirectoryExist "class/gpio/export") sysz [] `shouldBe` Right False
        it "returns False on non-existent names" $ do
          evalSysfsMock (doesDirectoryExist "/sys/class/foobar") sysfsRootZipper [] `shouldBe` Right False
          evalSysfsMock (doesDirectoryExist "foobar") sysz [] `shouldBe` Right False

      context "doesFileExist" $ do
        it "relative paths are relative to the initial zipper's working directory" $ do
          evalSysfsMock (doesFileExist "sys/class/gpio/export") sysfsRootZipper [] `shouldBe` Right True
          evalSysfsMock (doesFileExist "sys/class/gpio/export") sysz [] `shouldBe` Right False
          evalSysfsMock (doesFileExist "class/gpio/export") sysz [] `shouldBe` Right True
        it "absolute paths work regardless of the initial zipper's working directory" $ do
          evalSysfsMock (doesFileExist "/sys/class/gpio/export") sysfsRootZipper [] `shouldBe` Right True
          evalSysfsMock (doesFileExist "/sys/class/gpio/export") sysz [] `shouldBe` Right True
        it "doesn't change the initial zipper's state" $ do
          execSysfsMock (doesFileExist "/sys/class/gpio/export") sysfsRootZipper [] `shouldBe` Right defaultMockState
          execSysfsMock (doesFileExist "/sys/class/gpio/export") sysz [] `shouldBe` Right syszMockState
        it "returns False on directories" $ do
          evalSysfsMock (doesFileExist "/sys/class/gpio") sysfsRootZipper [] `shouldBe` Right False
          evalSysfsMock (doesFileExist "class/gpio") sysz [] `shouldBe` Right False
        it "returns False on non-existent names" $ do
          evalSysfsMock (doesFileExist "/sys/class/foobar") sysfsRootZipper [] `shouldBe` Right False
          evalSysfsMock (doesFileExist "foobar") sysz [] `shouldBe` Right False

      context "getDirectoryContents" $ do
        it "relative paths are relative to the initial zipper's working directory" $ do
          fmap sort (evalSysfsMock (getDirectoryContents "sys/class") sysfsRootZipper []) `shouldBe` Right ["gpio"]
          fmap sort (evalSysfsMock (getDirectoryContents "sys/class/gpio") sysfsRootZipper []) `shouldBe` (Right $ sort ["export", "unexport"])
          fmap sort (evalSysfsMock (getDirectoryContents "class") sysz []) `shouldBe` Right ["gpio"]
          fmap sort (evalSysfsMock (getDirectoryContents "class/gpio") sysz []) `shouldBe` (Right $ sort ["export", "unexport"])
          evalSysfsMock (getDirectoryContents "sys/class/gpio") sysz [] `shouldBe` Left (NoSuchFileOrDirectory "sys/class/gpio")
        it "absolute paths work regardless of the initial zipper's working directory" $ do
          fmap sort (evalSysfsMock (getDirectoryContents "/sys/class") sysfsRootZipper []) `shouldBe` Right ["gpio"]
          fmap sort (evalSysfsMock (getDirectoryContents "/sys/class/gpio") sysfsRootZipper []) `shouldBe` (Right $ sort ["export", "unexport"])
          fmap sort (evalSysfsMock (getDirectoryContents "/sys/class") sysz []) `shouldBe` Right ["gpio"]
          fmap sort (evalSysfsMock (getDirectoryContents "/sys/class/gpio") sysz []) `shouldBe` (Right $ sort ["export", "unexport"])
          fmap sort (evalSysfsMock (getDirectoryContents "/class") sysz []) `shouldBe` Left (NoSuchFileOrDirectory "/class")
        it "doesn't change the initial zipper's state" $ do
          execSysfsMock (getDirectoryContents "/sys/class/gpio") sysfsRootZipper [] `shouldBe` Right defaultMockState
          execSysfsMock (getDirectoryContents "/sys/class/gpio") sysz [] `shouldBe` Right syszMockState
        it "returns failure on files" $ do
          evalSysfsMock (getDirectoryContents "/sys/class/gpio/export") sysfsRootZipper [] `shouldBe` Left (NotADirectory "/sys/class/gpio/export")
          evalSysfsMock (getDirectoryContents "class/gpio/export") sysz [] `shouldBe` Left (NotADirectory "class/gpio/export")
        it "returns failure on non-existent names" $ do
          evalSysfsMock (getDirectoryContents "/sys/class/foobar") sysfsRootZipper [] `shouldBe` Left (NoSuchFileOrDirectory "/sys/class/foobar")
          evalSysfsMock (getDirectoryContents "sys/class/gpio") sysz [] `shouldBe` Left (NoSuchFileOrDirectory "sys/class/gpio")

      context "readFile" $ do
        it "fails on /sys/class/gpio/export" $
          evalSysfsMock (readFile "/sys/class/gpio/export") sysfsRootZipper [] `shouldBe` Left (ReadError "/sys/class/gpio/export")
        it "fails on /sys/class/gpio/unexport" $
          evalSysfsMock (readFile "/sys/class/gpio/unexport") sysfsRootZipper [] `shouldBe` Left (ReadError "/sys/class/gpio/unexport")
        it "fails on non-existent file" $
          evalSysfsMock (readFile "/sys/class/gpio/foo") sysfsRootZipper [] `shouldBe` Left (NotAFile "/sys/class/gpio/foo")
        it "fails on a directory" $
          evalSysfsMock (readFile "/sys/class/gpio") sysfsRootZipper [] `shouldBe` Left (NotAFile "/sys/class/gpio")

      context "runSysfsMockT" $ do
        let chip0 = MockGpioChip "chip0" 0 (replicate 16 defaultMockPinState)
            chip16 = MockGpioChip "xyz" 16 (replicate 32 defaultMockPinState)
            chip64 = MockGpioChip "abc" 64 (replicate 16 defaultMockPinState)
            invalidChip32 = MockGpioChip "invalid" 32 (replicate 16 defaultMockPinState)
        it "creates the specified gpiochip directories" $ do
          fmap sort (evalSysfsMock (getDirectoryContents "/sys/class/gpio") sysfsRootZipper [chip0, chip16, chip64]) `shouldBe` Right ["export", "gpiochip0", "gpiochip16", "gpiochip64", "unexport"]
          evalSysfsMock (readFile "/sys/class/gpio/gpiochip0/base") sysfsRootZipper [chip0, chip16, chip64] `shouldBe` Right "0\n"
          evalSysfsMock (readFile "/sys/class/gpio/gpiochip0/ngpio") sysfsRootZipper [chip0, chip16, chip64] `shouldBe` Right "16\n"
          evalSysfsMock (readFile "/sys/class/gpio/gpiochip0/label") sysfsRootZipper [chip0, chip16, chip64] `shouldBe` Right "chip0\n"
          evalSysfsMock (readFile "/sys/class/gpio/gpiochip16/base") sysfsRootZipper [chip0, chip16, chip64] `shouldBe` Right "16\n"
          evalSysfsMock (readFile "/sys/class/gpio/gpiochip16/ngpio") sysfsRootZipper [chip0, chip16, chip64] `shouldBe` Right "32\n"
          evalSysfsMock (readFile "/sys/class/gpio/gpiochip16/label") sysfsRootZipper [chip0, chip16, chip64] `shouldBe` Right "xyz\n"
          evalSysfsMock (readFile "/sys/class/gpio/gpiochip64/base") sysfsRootZipper [chip0, chip16, chip64] `shouldBe` Right "64\n"
          evalSysfsMock (readFile "/sys/class/gpio/gpiochip64/ngpio") sysfsRootZipper [chip0, chip16, chip64] `shouldBe` Right "16\n"
          evalSysfsMock (readFile "/sys/class/gpio/gpiochip64/label") sysfsRootZipper [chip0, chip16, chip64] `shouldBe` Right "abc\n"
          fmap sort (evalSysfsMock (getDirectoryContents "/sys/class/gpio") sysz [chip0, chip16, chip64]) `shouldBe` Right ["export", "gpiochip0", "gpiochip16", "gpiochip64", "unexport"]
        it "doesn't change the current working directory after creating gpiochip dirs" $ do
          fmap (dirName . cwd . _zipper) (execSysfsMock (getDirectoryContents "/") sysfsRootZipper [chip0]) `shouldBe` Right "/"
          fmap (dirName . cwd . _zipper) (execSysfsMock (getDirectoryContents "/") sysz [chip0]) `shouldBe` Right "sys"
        it "fails when MockGpioChips overlap" $ do
          evalSysfsMock (readFile "/sys/class/gpio/gpiochip16/ngpio") sysfsRootZipper [chip0, chip16, invalidChip32] `shouldBe` Left (PinAlreadyExists $ Pin 47)
