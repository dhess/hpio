{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.MockSpec (spec) where

import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.GPIO.Free
import System.GPIO.Mock

import Test.Hspec

-- To some extent, these tests only test the implementation of the
-- Mock interpreter, which isn't particularly valuable on its own.
-- However, it does allow us to test that we're able to express the
-- kinds of GpioT programs we want, in a pure environment (and when
-- the platform we're developing on doesn't actually have GPIO
-- functionality).

testOpenClose :: (MonadError String m) => MockT m ()
testOpenClose =
  do handle <- open (Pin 1)
     case handle of
       Left e -> throwError e
       Right d -> close d

toggleDirection :: PinDirection -> PinDirection
toggleDirection In = Out
toggleDirection Out = In

testSetDirection :: (MonadError String m) => (PinDirection -> PinDirection) -> MockT m PinDirection
testSetDirection f =
  do handle <- open (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do (Just dir1) <- direction d
            setDirection d (f dir1)
            (Just dir2) <- direction d
            close d
            return dir2

toggleValue :: Value -> Value
toggleValue Low = High
toggleValue High = Low

testReadWritePin :: (MonadError String m) => (Value -> Value) -> MockT m (Value, Value)
testReadWritePin f =
  do handle <- open (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do _ <- setDirection d Out
            val1 <- readPin d
            _ <- writePin d (f val1)
            val2 <- readPin d
            close d
            return (val1, val2)

testWritePinFailsOnInputPin :: (MonadError String m) => MockT m ()
testWritePinFailsOnInputPin =
  do handle <- open (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do _ <- setDirection d In
            _ <- writePin d High
            close d

invalidHandle :: (MonadError String m) => (MockHandle -> MockT m a) -> MockT m a
invalidHandle action =
  do handle <- open (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do close d
            action d

spec :: Spec
spec =
  do describe "open and close" $

       do it "succeeds when the pin is available" $
            do let expectedResult = (Right (), Map.empty, ["Open Pin 1", "Close MockHandle (Pin 1)"])
               runMock (Set.fromList [Pin 1]) testOpenClose `shouldBe` expectedResult

          it "fails when the pin is unavailable" $
            do let expectedResult = (Left "Open failed: Pin 1 does not exist", Map.empty, [])
               runMock (Set.fromList [Pin 2]) testOpenClose `shouldBe` expectedResult

     describe "setDirection" $

       do it "toggles pin direction" $
            let expectedResult = (Right Out, Map.empty, ["Open Pin 1", "Set direction: MockHandle (Pin 1) Out", "Close MockHandle (Pin 1)"])
            in runMock (Set.fromList [Pin 1]) (testSetDirection toggleDirection) `shouldBe` expectedResult

          it "is idempotent" $
            let expectedResult = (Right In, Map.empty, ["Open Pin 1", "Set direction: MockHandle (Pin 1) In", "Close MockHandle (Pin 1)"])
            in runMock (Set.fromList [Pin 1]) (testSetDirection id) `shouldBe` expectedResult

     describe "writePin" $
       do it "toggles pin value" $
            let expectedResult = (Right (Low, High), Map.empty, ["Open Pin 1", "Set direction: MockHandle (Pin 1) Out", "Write: MockHandle (Pin 1) High", "Close MockHandle (Pin 1)"])
            in runMock (Set.fromList [Pin 1]) (testReadWritePin toggleValue) `shouldBe` expectedResult

          it "is idempotent" $
            let expectedResult = (Right (Low, Low), Map.empty, ["Open Pin 1", "Set direction: MockHandle (Pin 1) Out", "Write: MockHandle (Pin 1) Low", "Close MockHandle (Pin 1)"])
            in runMock (Set.fromList [Pin 1]) (testReadWritePin id) `shouldBe` expectedResult

          it "fails when the pin direction is In" $
            -- Note that this test will leave Pin 1 in the "open" state.
            let expectedResult = (Left "MockHandle (Pin 1) is configured for input", Map.fromList [(MockHandle (Pin 1),PinState {dir = In, value = Low})], ["Open Pin 1", "Set direction: MockHandle (Pin 1) In"])
            in runMock (Set.fromList [Pin 1]) testWritePinFailsOnInputPin `shouldBe` expectedResult

     describe "invalid handles throw exceptions" $
       do it "in direction" $
            let expectedResult = (Left "Pin handle MockHandle (Pin 1) is invalid", Map.empty, ["Open Pin 1", "Close MockHandle (Pin 1)"])
            in
              runMock (Set.fromList [Pin 1]) (invalidHandle direction) `shouldBe` expectedResult

          let expectedResult = (Left "Pin handle MockHandle (Pin 1) is invalid", Map.empty, ["Open Pin 1", "Close MockHandle (Pin 1)"])

          it "in setDirection" $
            runMock (Set.fromList [Pin 1]) (invalidHandle (\d -> setDirection d Out)) `shouldBe` expectedResult

          it "in readPin" $
            runMock (Set.fromList [Pin 1]) (invalidHandle readPin) `shouldBe` expectedResult

          it "in writePin" $
            runMock (Set.fromList [Pin 1]) (invalidHandle (\d -> writePin d High)) `shouldBe` expectedResult
