{-# LANGUAGE OverloadedStrings #-}

module System.GPIOSpec (spec) where

import System.GPIO.Free
import System.GPIO.Mock

import Test.Hspec

testAllocDealloc :: (Monad m) => GpioT m ()
testAllocDealloc =
  do Just descriptor <- open (Pin 1)
     close descriptor

spec :: Spec
spec =
  do describe "runMock" $
       do it "produces the right result" $
            do let expectedResult = ((), ["Open Pin 1", "Close PinDescriptor (Pin 1)"])
               runMock testAllocDealloc `shouldBe` expectedResult
