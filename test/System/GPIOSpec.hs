{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIOSpec (spec) where

import Control.Monad.Except
import qualified Data.Set as Set
import System.GPIO.Free
import System.GPIO.Mock

import Test.Hspec

testOpenClose :: (MonadError String m) => (GpioT String) m ()
testOpenClose =
  do descriptor <- open (Pin 1)
     case descriptor of
       Left e -> throwError e
       Right d -> close d

spec :: Spec
spec =
  do describe "open and close" $

       do it "succeeds when the pin is available" $
            do let expectedResult = (Right (), emptyWorld, ["Open Pin 1", "Close PinDescriptor (Pin 1)"])
               runMock (Env $ Set.fromList [Pin 1]) testOpenClose `shouldBe` expectedResult

          it "fails when the pin is unavailable" $
            do let expectedResult = (Left "Open failed: Pin 1 does not exist", emptyWorld, [])
               runMock (Env $ Set.fromList [Pin 2]) testOpenClose `shouldBe` expectedResult
