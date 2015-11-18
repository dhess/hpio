{-# LANGUAGE OverloadedStrings #-}

module System.GPIOSpec (spec) where

import Control.Monad (liftM)
import qualified Data.Set as Set
import System.GPIO.Free
import System.GPIO.Mock

import Test.Hspec

testAllocDealloc :: (Monad m) => GpioT m ()
testAllocDealloc =
  do descriptor <- open (Pin 1)
     case descriptor of
       (Just d) -> close d
       _ -> return ()

spec :: Spec
spec =
  do describe "runMock" $
       do it "produces the right result" $
            do let expectedResult = ((), ["Open Pin 1", "Close PinDescriptor (Pin 1)"])
               runMock (Env $ Set.fromList [Pin 1, Pin 2]) testAllocDealloc `shouldBe` expectedResult
