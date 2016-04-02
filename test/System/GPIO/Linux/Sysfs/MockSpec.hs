{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.MockSpec (spec) where

import Prelude hiding (readFile, writeFile)
import Control.Exception (fromException)
import Data.List (sort)
import System.GPIO.Linux.Sysfs.Mock
import System.GPIO.Types (Pin(..), PinValue(..))
import Test.Hspec

evalSysfsMock' :: SysfsMock a -> MockWorld -> [MockGpioChip] -> Either (Maybe MockFSException) a
evalSysfsMock' a w c =
  case evalSysfsMock a w c of
    Right x -> return x
    Left e -> Left $ fromException e

execSysfsMock' :: SysfsMock a -> MockWorld -> [MockGpioChip] -> Either (Maybe MockFSException) MockWorld
execSysfsMock' a w c =
  case execSysfsMock a w c of
    Right x -> return x
    Left e -> Left $ fromException e

spec :: Spec
spec =
  do
    describe "MockPinState" $ do
      it "logicalValue returns the correct pin value" $
        let pinState = defaultMockPinState {_value = Low, _activeLow = False}
        in do
          logicalValue pinState `shouldBe` Low
          logicalValue (pinState {_value = High}) `shouldBe` High
          logicalValue (pinState {_activeLow = True}) `shouldBe` High
          logicalValue (pinState {_value = High, _activeLow = True}) `shouldBe` Low
      it "setLogicalValue sets the correct pin value" $
        let pinState = defaultMockPinState {_value = Low, _activeLow = False}
            activeLowPinState = defaultMockPinState {_value = Low, _activeLow = True}
        in do
          setLogicalValue Low pinState `shouldBe` pinState
          setLogicalValue High pinState `shouldBe` pinState {_value = High}
          setLogicalValue Low activeLowPinState `shouldBe` activeLowPinState {_value = High}
          setLogicalValue High activeLowPinState `shouldBe` activeLowPinState {_value = Low}

    describe "SysfsMockT" $ do

      context "doesDirectoryExist" $ do
        it "relative paths are relative to the initial zipper's working directory" $ do
          evalSysfsMock' (doesDirectoryExist "sys/class/gpio") initialMockWorld [] `shouldBe` Right True
        it "absolute paths work regardless of the initial zipper's working directory" $ do
          evalSysfsMock' (doesDirectoryExist "/sys/class/gpio") initialMockWorld [] `shouldBe` Right True
        it "doesn't change the initial zipper's state" $ do
          execSysfsMock' (doesDirectoryExist "/sys/class/gpio") initialMockWorld [] `shouldBe` Right initialMockWorld
        it "returns False on files" $ do
          evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/export") initialMockWorld [] `shouldBe` Right False
        it "returns False on non-existent names" $ do
          evalSysfsMock' (doesDirectoryExist "/sys/class/foobar") initialMockWorld [] `shouldBe` Right False

      context "doesFileExist" $ do
        it "relative paths are relative to the initial zipper's working directory" $ do
          evalSysfsMock' (doesFileExist "sys/class/gpio/export") initialMockWorld [] `shouldBe` Right True
        it "absolute paths work regardless of the initial zipper's working directory" $ do
          evalSysfsMock' (doesFileExist "/sys/class/gpio/export") initialMockWorld [] `shouldBe` Right True
        it "doesn't change the initial zipper's state" $ do
          execSysfsMock' (doesFileExist "/sys/class/gpio/export") initialMockWorld [] `shouldBe` Right initialMockWorld
        it "returns False on directories" $ do
          evalSysfsMock' (doesFileExist "/sys/class/gpio") initialMockWorld [] `shouldBe` Right False
        it "returns False on non-existent names" $ do
          evalSysfsMock' (doesFileExist "/sys/class/foobar") initialMockWorld [] `shouldBe` Right False

      context "getDirectoryContents" $ do
        it "relative paths are relative to the initial zipper's working directory" $ do
          fmap sort (evalSysfsMock' (getDirectoryContents "sys/class") initialMockWorld []) `shouldBe` Right ["gpio"]
          fmap sort (evalSysfsMock' (getDirectoryContents "sys/class/gpio") initialMockWorld []) `shouldBe` (Right $ sort ["export", "unexport"])
        it "absolute paths work regardless of the initial zipper's working directory" $ do
          fmap sort (evalSysfsMock' (getDirectoryContents "/sys/class") initialMockWorld []) `shouldBe` Right ["gpio"]
          fmap sort (evalSysfsMock' (getDirectoryContents "/sys/class/gpio") initialMockWorld []) `shouldBe` (Right $ sort ["export", "unexport"])
        it "doesn't change the initial zipper's state" $ do
          execSysfsMock' (getDirectoryContents "/sys/class/gpio") initialMockWorld [] `shouldBe` Right initialMockWorld
        it "returns failure on files" $ do
          evalSysfsMock' (getDirectoryContents "/sys/class/gpio/export") initialMockWorld [] `shouldBe` Left (Just $ NotADirectory "/sys/class/gpio/export")
        it "returns failure on non-existent names" $ do
          evalSysfsMock' (getDirectoryContents "/sys/class/foobar") initialMockWorld [] `shouldBe` Left (Just $ NoSuchFileOrDirectory "/sys/class/foobar")

      context "readFile" $ do
        it "fails on /sys/class/gpio/export" $
          evalSysfsMock' (readFile "/sys/class/gpio/export") initialMockWorld [] `shouldBe` Left (Just $ WriteOnlyFile "/sys/class/gpio/export")
        it "fails on /sys/class/gpio/unexport" $
          evalSysfsMock' (readFile "/sys/class/gpio/unexport") initialMockWorld [] `shouldBe` Left (Just $ WriteOnlyFile "/sys/class/gpio/unexport")
        it "fails on non-existent file" $
          evalSysfsMock' (readFile "/sys/class/gpio/foo") initialMockWorld [] `shouldBe` Left (Just $ NotAFile "/sys/class/gpio/foo")
        it "fails on a directory" $
          evalSysfsMock' (readFile "/sys/class/gpio") initialMockWorld [] `shouldBe` Left (Just $ NotAFile "/sys/class/gpio")

      context "runSysfsMockT" $ do
        let chip0 = MockGpioChip "chip0" 0 (replicate 16 defaultMockPinState)
            chip16 = MockGpioChip "xyz" 16 (replicate 32 defaultMockPinState)
            chip64 = MockGpioChip "abc" 64 (replicate 16 defaultMockPinState)
            invalidChip32 = MockGpioChip "invalid" 32 (replicate 16 defaultMockPinState)
        it "creates the specified gpiochip directories" $ do
          fmap sort (evalSysfsMock' (getDirectoryContents "/sys/class/gpio") initialMockWorld [chip0, chip16, chip64]) `shouldBe` Right ["export", "gpiochip0", "gpiochip16", "gpiochip64", "unexport"]
          evalSysfsMock' (readFile "/sys/class/gpio/gpiochip0/base") initialMockWorld [chip0, chip16, chip64] `shouldBe` Right "0\n"
          evalSysfsMock' (readFile "/sys/class/gpio/gpiochip0/ngpio") initialMockWorld [chip0, chip16, chip64] `shouldBe` Right "16\n"
          evalSysfsMock' (readFile "/sys/class/gpio/gpiochip0/label") initialMockWorld [chip0, chip16, chip64] `shouldBe` Right "chip0\n"
          evalSysfsMock' (readFile "/sys/class/gpio/gpiochip16/base") initialMockWorld [chip0, chip16, chip64] `shouldBe` Right "16\n"
          evalSysfsMock' (readFile "/sys/class/gpio/gpiochip16/ngpio") initialMockWorld [chip0, chip16, chip64] `shouldBe` Right "32\n"
          evalSysfsMock' (readFile "/sys/class/gpio/gpiochip16/label") initialMockWorld [chip0, chip16, chip64] `shouldBe` Right "xyz\n"
          evalSysfsMock' (readFile "/sys/class/gpio/gpiochip64/base") initialMockWorld [chip0, chip16, chip64] `shouldBe` Right "64\n"
          evalSysfsMock' (readFile "/sys/class/gpio/gpiochip64/ngpio") initialMockWorld [chip0, chip16, chip64] `shouldBe` Right "16\n"
          evalSysfsMock' (readFile "/sys/class/gpio/gpiochip64/label") initialMockWorld [chip0, chip16, chip64] `shouldBe` Right "abc\n"
        it "fails when MockGpioChips overlap" $ do
          evalSysfsMock' (readFile "/sys/class/gpio/gpiochip16/ngpio") initialMockWorld [chip0, chip16, invalidChip32] `shouldBe` Left (Just $ PinAlreadyExists $ Pin 47)
