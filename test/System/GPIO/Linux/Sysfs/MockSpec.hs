{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.MockSpec (spec) where

import Data.List (sort)
import System.GPIO.Linux.Sysfs.Mock
import System.GPIO.Linux.Sysfs.Mock.Internal (cd)
import Test.Hspec


spec :: Spec
spec =
  let rootz = (sysfsRoot, [])
      Right sysz = cd "/sys" rootz
  in do
    describe "SysfsMockT" $ do
      context "doesDirectoryExist" $ do
        it "relative paths are relative to the initial zipper's working directory" $ do
          evalSysfsMock (doesDirectoryExist "sys/class/gpio") rootz `shouldBe` Right True
          evalSysfsMock (doesDirectoryExist "sys/class/gpio") sysz `shouldBe` Right False
          evalSysfsMock (doesDirectoryExist "class/gpio") sysz `shouldBe` Right True
        it "absolute paths work regardless of the initial zipper's working directory" $ do
          evalSysfsMock (doesDirectoryExist "/sys/class/gpio") rootz `shouldBe` Right True
          evalSysfsMock (doesDirectoryExist "/sys/class/gpio") sysz `shouldBe` Right True
        it "doesn't change the initial zipper's state" $ do
          execSysfsMock (doesDirectoryExist "/sys/class/gpio") rootz `shouldBe` Right rootz
          execSysfsMock (doesDirectoryExist "/sys/class/gpio") sysz `shouldBe` Right sysz
        it "returns False on files" $ do
          evalSysfsMock (doesDirectoryExist "/sys/class/gpio/export") rootz `shouldBe` Right False
          evalSysfsMock (doesDirectoryExist "class/gpio/export") sysz `shouldBe` Right False
        it "returns False on non-existent names" $ do
          evalSysfsMock (doesDirectoryExist "/sys/class/foobar") rootz `shouldBe` Right False
          evalSysfsMock (doesDirectoryExist "foobar") sysz `shouldBe` Right False
      context "doesFileExist" $ do
        it "relative paths are relative to the initial zipper's working directory" $ do
          evalSysfsMock (doesFileExist "sys/class/gpio/export") rootz `shouldBe` Right True
          evalSysfsMock (doesFileExist "sys/class/gpio/export") sysz `shouldBe` Right False
          evalSysfsMock (doesFileExist "class/gpio/export") sysz `shouldBe` Right True
        it "absolute paths work regardless of the initial zipper's working directory" $ do
          evalSysfsMock (doesFileExist "/sys/class/gpio/export") rootz `shouldBe` Right True
          evalSysfsMock (doesFileExist "/sys/class/gpio/export") sysz `shouldBe` Right True
        it "doesn't change the initial zipper's state" $ do
          execSysfsMock (doesFileExist "/sys/class/gpio/export") rootz `shouldBe` Right rootz
          execSysfsMock (doesFileExist "/sys/class/gpio/export") sysz `shouldBe` Right sysz
        it "returns False on directories" $ do
          evalSysfsMock (doesFileExist "/sys/class/gpio") rootz `shouldBe` Right False
          evalSysfsMock (doesFileExist "class/gpio") sysz `shouldBe` Right False
        it "returns False on non-existent names" $ do
          evalSysfsMock (doesFileExist "/sys/class/foobar") rootz `shouldBe` Right False
          evalSysfsMock (doesFileExist "foobar") sysz `shouldBe` Right False
      context "getDirectoryContents" $ do
        it "relative paths are relative to the initial zipper's working directory" $ do
          fmap sort (evalSysfsMock (getDirectoryContents "sys/class") rootz) `shouldBe` Right ["gpio"]
          fmap sort (evalSysfsMock (getDirectoryContents "sys/class/gpio") rootz) `shouldBe` (Right $ sort ["export", "unexport"])
          fmap sort (evalSysfsMock (getDirectoryContents "class") sysz) `shouldBe` Right ["gpio"]
          fmap sort (evalSysfsMock (getDirectoryContents "class/gpio") sysz) `shouldBe` (Right $ sort ["export", "unexport"])
          evalSysfsMock (getDirectoryContents "sys/class/gpio") sysz `shouldBe` Left (NoSuchFileOrDirectory "sys/class/gpio")
        it "absolute paths work regardless of the initial zipper's working directory" $ do
          fmap sort (evalSysfsMock (getDirectoryContents "/sys/class") rootz) `shouldBe` Right ["gpio"]
          fmap sort (evalSysfsMock (getDirectoryContents "/sys/class/gpio") rootz) `shouldBe` (Right $ sort ["export", "unexport"])
          fmap sort (evalSysfsMock (getDirectoryContents "/sys/class") sysz) `shouldBe` Right ["gpio"]
          fmap sort (evalSysfsMock (getDirectoryContents "/sys/class/gpio") sysz) `shouldBe` (Right $ sort ["export", "unexport"])
          fmap sort (evalSysfsMock (getDirectoryContents "/class") sysz) `shouldBe` Left (NoSuchFileOrDirectory "/class")
        it "doesn't change the initial zipper's state" $ do
          execSysfsMock (getDirectoryContents "/sys/class/gpio") rootz `shouldBe` Right rootz
          execSysfsMock (getDirectoryContents "/sys/class/gpio") sysz `shouldBe` Right sysz
        it "returns failure on files" $ do
          evalSysfsMock (getDirectoryContents "/sys/class/gpio/export") rootz `shouldBe` Left (NotADirectory "/sys/class/gpio/export")
          evalSysfsMock (getDirectoryContents "class/gpio/export") sysz `shouldBe` Left (NotADirectory "class/gpio/export")
        it "returns failure on non-existent names" $ do
          evalSysfsMock (getDirectoryContents "/sys/class/foobar") rootz `shouldBe` Left (NoSuchFileOrDirectory "/sys/class/foobar")
          evalSysfsMock (getDirectoryContents "sys/class/gpio") sysz `shouldBe` Left (NoSuchFileOrDirectory "sys/class/gpio")
