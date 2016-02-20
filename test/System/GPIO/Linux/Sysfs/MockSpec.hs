{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.MockSpec (spec) where

import System.GPIO.Linux.Sysfs.Mock
import Test.Hspec

spec :: Spec
spec =
  let rootz = (sysfsRoot, [])
      Right sysz = cd "/sys" rootz
  in do
    describe "SysfsMockT" $ do
      context "doesDirectoryExist" $ do
        it "relative paths are relative to the initial zipper's working directory" $ do
          evalSysfsMock (doesDirectoryExist "sys/class/gpio") rootz `shouldBe` True
          evalSysfsMock (doesDirectoryExist "sys/class/gpio") sysz `shouldBe` False
          evalSysfsMock (doesDirectoryExist "class/gpio") sysz `shouldBe` True
        it "absolute paths work regardless of the initial zipper's working directory" $ do
          evalSysfsMock (doesDirectoryExist "/sys/class/gpio") rootz `shouldBe` True
          evalSysfsMock (doesDirectoryExist "/sys/class/gpio") sysz `shouldBe` True
        it "doesn't change the initial zipper's state" $ do
          execSysfsMock (doesDirectoryExist "/sys/class/gpio") rootz `shouldBe` rootz
          execSysfsMock (doesDirectoryExist "/sys/class/gpio") sysz `shouldBe` sysz
        it "returns False on files" $ do
          evalSysfsMock (doesDirectoryExist "/sys/class/gpio/export") rootz `shouldBe` False
          evalSysfsMock (doesDirectoryExist "class/gpio/export") sysz `shouldBe` False
        it "returns False on non-existent names" $ do
          evalSysfsMock (doesDirectoryExist "/sys/class/foobar") rootz `shouldBe` False
          evalSysfsMock (doesDirectoryExist "foobar") sysz `shouldBe` False
