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

toggleDirection :: Direction -> Direction
toggleDirection In = Out
toggleDirection Out = In

testGetSetDirection :: (MonadError String m) => (Direction -> Direction) -> (GpioT String) m Direction
testGetSetDirection f =
  do descriptor <- open (Pin 1)
     case descriptor of
       Left e -> throwError e
       Right d ->
         do dir1 <- getDirection d
            setDirection d (f dir1)
            dir2 <- getDirection d
            close d
            return dir2

toggleValue :: Value -> Value
toggleValue Low = High
toggleValue High = Low

testReadWritePin :: (MonadError String m) => (Value -> Value) -> (GpioT String) m (Value, Value)
testReadWritePin f =
  do descriptor <- open (Pin 1)
     case descriptor of
       Left e -> throwError e
       Right d ->
         do _ <- setDirection d Out
            val1 <- readPin d
            _ <- writePin d (f val1)
            val2 <- readPin d
            close d
            return (val1, val2)

testWritePinFailsOnInputPin :: (MonadError String m) => (GpioT String) m ()
testWritePinFailsOnInputPin =
  do descriptor <- open (Pin 1)
     case descriptor of
       Left e -> throwError e
       Right d ->
         do _ <- setDirection d In
            _ <- writePin d High
            close d

spec :: Spec
spec =
  do describe "open and close" $

       do it "succeeds when the pin is available" $
            do let expectedResult = (Right (), Map.empty, ["Open Pin 1", "Close PinDescriptor (Pin 1)"])
               runMock (Set.fromList [Pin 1]) testOpenClose `shouldBe` expectedResult

          it "fails when the pin is unavailable" $
            do let expectedResult = (Left "Open failed: Pin 1 does not exist", Map.empty, [])
               runMock (Set.fromList [Pin 2]) testOpenClose `shouldBe` expectedResult

     describe "setDirection" $

       do it "toggles pin direction" $
            let expectedResult = (Right Out, Map.empty, ["Open Pin 1", "Set direction: PinDescriptor (Pin 1) Out", "Close PinDescriptor (Pin 1)"])
            in runMock (Set.fromList [Pin 1]) (testGetSetDirection toggleDirection) `shouldBe` expectedResult

          it "is idempotent" $
            let expectedResult = (Right In, Map.empty, ["Open Pin 1", "Set direction: PinDescriptor (Pin 1) In", "Close PinDescriptor (Pin 1)"])
            in runMock (Set.fromList [Pin 1]) (testGetSetDirection id) `shouldBe` expectedResult

     describe "writePin" $
       do it "toggles pin value" $
            let expectedResult = (Right (Low, High), Map.empty, ["Open Pin 1", "Set direction: PinDescriptor (Pin 1) Out", "Write: PinDescriptor (Pin 1) High", "Close PinDescriptor (Pin 1)"])
            in runMock (Set.fromList [Pin 1]) (testReadWritePin toggleValue) `shouldBe` expectedResult

          it "is idempotent" $
            let expectedResult = (Right (Low, Low), Map.empty, ["Open Pin 1", "Set direction: PinDescriptor (Pin 1) Out", "Write: PinDescriptor (Pin 1) Low", "Close PinDescriptor (Pin 1)"])
            in runMock (Set.fromList [Pin 1]) (testReadWritePin id) `shouldBe` expectedResult

          it "fails when the pin direction is In" $
            -- Note that this test will leave Pin 1 in the "open" state.
            let expectedResult = (Left "PinDescriptor (Pin 1) is configured for input", Map.fromList [(PinDescriptor (Pin 1),PinState {direction = In, value = Low})], ["Open Pin 1", "Set direction: PinDescriptor (Pin 1) In"])
            in runMock (Set.fromList [Pin 1]) testWritePinFailsOnInputPin `shouldBe` expectedResult
