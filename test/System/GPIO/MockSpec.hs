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

testOpenClose :: (MonadError e m) => GpioT e h m m ()
testOpenClose =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d -> closePin d

toggleDirection :: PinDirection -> PinDirection
toggleDirection In = Out
toggleDirection Out = In

testSetDirection :: (MonadError e m) => (PinDirection -> PinDirection) -> GpioT e h m m PinDirection
testSetDirection f =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do (Just dir1) <- getPinDirection d
            setPinDirection d (f dir1)
            (Just dir2) <- getPinDirection d
            closePin d
            return dir2

toggleValue :: Value -> Value
toggleValue Low = High
toggleValue High = Low

testReadWritePin :: (MonadError e m) => (Value -> Value) -> GpioT e h m m (Value, Value)
testReadWritePin f =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do void $ setPinDirection d Out
            val1 <- readPin d
            void $ writePin d (f val1)
            val2 <- readPin d
            closePin d
            return (val1, val2)

testWritePinFailsOnInputPin :: (MonadError e m) => GpioT e h m m ()
testWritePinFailsOnInputPin =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do void $ setPinDirection d In
            void $ writePin d High
            closePin d

invalidHandle :: (MonadError e m) => (h -> GpioT e h m m a) -> GpioT e h m m a
invalidHandle action =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do closePin d
            action d

testWithPin :: (MonadError e m) => GpioT e h m m Value
testWithPin = withPin (Pin 1) $ \h ->
  do void $ setPinDirection h Out
     void $ writePin h High
     val <- readPin h
     return val

testWithPinError :: (MonadError e m) => GpioT e h m m Value
testWithPinError = withPin (Pin 1) $ \h ->
  do void $ setPinDirection h In
     void $ writePin h High -- should fail
     val <- readPin h
     return val

testNestedWithPin :: (MonadError e m) => GpioT e h m m (Value, Value)
testNestedWithPin =
  withPin (Pin 1) $ \h1 ->
    withPin (Pin 2) $ \h2 ->
      do void $ setPinDirection h1 Out
         void $ setPinDirection h2 Out
         void $ writePin h1 High
         void $ writePin h2 Low
         val1 <- readPin h1
         val2 <- readPin h2
         return (val1, val2)

testNestedWithPinError :: (MonadError e m) => GpioT e h m m (Value, Value)
testNestedWithPinError =
  withPin (Pin 1) $ \h1 ->
    withPin (Pin 2) $ \h2 ->
      do void $ setPinDirection h1 Out
         void $ setPinDirection h2 In
         void $ writePin h1 High
         void $ writePin h2 Low -- should fail
         val1 <- readPin h1
         val2 <- readPin h2
         return (val1, val2)

spec :: Spec
spec =
  do describe "pins" $
       it "returns the list of available pins" $
         let pinList = [Pin 1, Pin 8]
             expectedResult = (Right pinList, Map.empty, [])
         in runMock (Set.fromList pinList) pins `shouldBe` expectedResult

     describe "openPin and closePin" $

       do it "succeeds when the pin is available" $
            do let expectedResult = (Right (), Map.empty, ["Open Pin 1", "Close MockHandle (Pin 1)"])
               runMock (Set.fromList [Pin 1]) testOpenClose `shouldBe` expectedResult

          it "fails when the pin is unavailable" $
            do let expectedResult = (Left "Open failed: Pin 1 does not exist", Map.empty, [])
               runMock (Set.fromList [Pin 2]) testOpenClose `shouldBe` expectedResult

     describe "setPinDirection" $

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
            let expectedResult = (Left "MockHandle (Pin 1) is configured for input", Map.fromList [(MockHandle (Pin 1),MockState {dir = In, value = Low})], ["Open Pin 1", "Set direction: MockHandle (Pin 1) In"])
            in runMock (Set.fromList [Pin 1]) testWritePinFailsOnInputPin `shouldBe` expectedResult

     describe "invalid handles throw exceptions" $
       do it "in getPinDirection" $
            let expectedResult = (Left "Pin handle MockHandle (Pin 1) is invalid", Map.empty, ["Open Pin 1", "Close MockHandle (Pin 1)"])
            in
              runMock (Set.fromList [Pin 1]) (invalidHandle getPinDirection) `shouldBe` expectedResult

          let expectedResult = (Left "Pin handle MockHandle (Pin 1) is invalid", Map.empty, ["Open Pin 1", "Close MockHandle (Pin 1)"])

          it "in setPinDirection" $
            runMock (Set.fromList [Pin 1]) (invalidHandle (\d -> setPinDirection d Out)) `shouldBe` expectedResult

          it "in readPin" $
            runMock (Set.fromList [Pin 1]) (invalidHandle readPin) `shouldBe` expectedResult

          it "in writePin" $
            runMock (Set.fromList [Pin 1]) (invalidHandle (\d -> writePin d High)) `shouldBe` expectedResult

     describe "withPin" $
       do it "opens and closes the pin as expected" $
            let expectedResult = (Right High, Map.empty, ["Open Pin 1", "Set direction: MockHandle (Pin 1) Out", "Write: MockHandle (Pin 1) High", "Close MockHandle (Pin 1)"])
            in runMock (Set.fromList [Pin 1]) testWithPin `shouldBe` expectedResult

          it "throws an exception when the pin does not exist" $
            let expectedResult = (Left "Open failed: Pin 1 does not exist", Map.empty, [])
            in runMock (Set.fromList [Pin 2]) testWithPin `shouldBe` expectedResult

          it "closes the pin when an exception occurs inside the block" $
            let expectedResult = (Left "MockHandle (Pin 1) is configured for input", Map.empty, ["Open Pin 1", "Set direction: MockHandle (Pin 1) In", "Close MockHandle (Pin 1)"])
            in runMock (Set.fromList [Pin 1]) testWithPinError `shouldBe` expectedResult


          it "can nest" $
            let expectedResult = (Right (High, Low), Map.empty, ["Open Pin 1", "Open Pin 2", "Set direction: MockHandle (Pin 1) Out", "Set direction: MockHandle (Pin 2) Out", "Write: MockHandle (Pin 1) High", "Write: MockHandle (Pin 2) Low", "Close MockHandle (Pin 2)", "Close MockHandle (Pin 1)"])
            in runMock (Set.fromList [Pin 1, Pin 2]) testNestedWithPin `shouldBe` expectedResult

          it "fails properly when nested" $
            let expectedResult1 = (Left "Open failed: Pin 2 does not exist", Map.empty, ["Open Pin 1", "Close MockHandle (Pin 1)"])
                expectedResult2 = (Left "MockHandle (Pin 2) is configured for input", Map.empty, ["Open Pin 1", "Open Pin 2", "Set direction: MockHandle (Pin 1) Out", "Set direction: MockHandle (Pin 2) In", "Write: MockHandle (Pin 1) High", "Close MockHandle (Pin 2)", "Close MockHandle (Pin 1)"])
            in
              do runMock (Set.fromList [Pin 1]) testNestedWithPin `shouldBe` expectedResult1
                 runMock (Set.fromList [Pin 1, Pin 2]) testNestedWithPinError `shouldBe` expectedResult2
