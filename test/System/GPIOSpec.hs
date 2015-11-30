{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIOSpec (spec) where

import Control.Monad.Except
import qualified Data.Map as Map
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

testGetSetDirection :: (MonadError String m) => (GpioT String) m Direction
testGetSetDirection =
  do descriptor <- open (Pin 1)
     case descriptor of
       Left e -> throwError e
       Right d ->
         do dir1 <- getDirection d
            case dir1 of
              In -> setDirection d Out
              Out -> setDirection d In
            dir2 <- getDirection d
            close d
            return dir2

spec :: Spec
spec =
  do describe "open and close" $

       do it "succeeds when the pin is available" $
            do let expectedResult = (Right (), Map.empty, ["Open Pin 1", "Close PinDescriptor (Pin 1)"])
               runMock (Set.fromList [Pin 1]) testOpenClose `shouldBe` expectedResult

          it "fails when the pin is unavailable" $
            do let expectedResult = (Left "Open failed: Pin 1 does not exist", Map.empty, [])
               runMock (Set.fromList [Pin 2]) testOpenClose `shouldBe` expectedResult

     describe "getDirection and setDirection" $

       do it "gets and sets pin input/output direction" $
            let expectedResult = (Right Out, Map.empty, ["Open Pin 1", "Set direction: PinDescriptor (Pin 1) Out", "Close PinDescriptor (Pin 1)"])
            in runMock (Set.fromList [Pin 1]) testGetSetDirection `shouldBe` expectedResult
