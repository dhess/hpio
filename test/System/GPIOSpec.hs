{-# LANGUAGE OverloadedStrings #-}

module System.GPIOSpec (spec) where

import Control.Monad.Writer
import System.GPIO.Free
import System.GPIO.Mock

import Test.Hspec

testAllocDealloc :: (Monad m) => GpioT m ()
testAllocDealloc =
  do allocPin (Pin 1)
     deallocPin (Pin 1)

spec :: Spec
spec =
  do describe "runMock" $
       do it "produces the right result" $
            do let expectedResult = ["Alloc Pin 1", "Dealloc Pin 1"]
               execWriter (runMock testAllocDealloc) `shouldBe` expectedResult
