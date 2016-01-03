{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.SysfsSpec (spec) where

import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.IO.Exception (IOErrorType(..), IOException(ioe_type))
import System.GPIO.Free
import System.GPIO.Linux.Sysfs
import System.GPIO.Linux.SysfsMock
import System.IO.Error

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

testSetDirection :: (MonadError e m) => GpioT e h m m (PinDirection, PinDirection, PinDirection)
testSetDirection =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do (Just dir1) <- getPinDirection d
            case dir1 of
              In ->
                do void $ setPinDirection d Out
                   (Just dir2) <- getPinDirection d
                   void $ setPinDirection d In
                   (Just dir3) <- getPinDirection d
                   closePin d
                   return (dir1, dir2, dir3)
              Out ->
                do void $ setPinDirection d In
                   (Just dir2) <- getPinDirection d
                   void $ setPinDirection d Out
                   (Just dir3) <- getPinDirection d
                   closePin d
                   return (dir1, dir2, dir3)

testSetDirectionIdempotent :: (MonadError e m) => GpioT e h m m (PinDirection, PinDirection)
testSetDirectionIdempotent =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do (Just dir1) <- getPinDirection d
            case dir1 of
              In ->
                do void $ setPinDirection d In
                   (Just dir2) <- getPinDirection d
                   closePin d
                   return (dir1, dir2)
              Out ->
                do void $ setPinDirection d Out
                   (Just dir2) <- getPinDirection d
                   closePin d
                   return (dir1, dir2)

testTogglePinDirection :: (MonadError e m) => GpioT e h m m (PinDirection, PinDirection, PinDirection, PinDirection, PinDirection)
testTogglePinDirection =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do (Just dir1) <- getPinDirection d
            (Just dir2) <- togglePinDirection d
            (Just dir3) <- getPinDirection d
            (Just dir4) <- togglePinDirection d
            (Just dir5) <- getPinDirection d
            closePin d
            return (dir1, dir2, dir3, dir4, dir5)

testReadWritePin :: (MonadError e m) => GpioT e h m m (PinValue, PinValue, PinValue)
testReadWritePin =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do void $ setPinDirection d Out
            val1 <- readPin d
            case val1 of
              Low ->
                do void $ writePin d High
                   val2 <- readPin d
                   void $ writePin d Low
                   val3 <- readPin d
                   closePin d
                   return (val1, val2, val3)
              High ->
                do void $ writePin d Low
                   val2 <- readPin d
                   void $ writePin d High
                   val3 <- readPin d
                   closePin d
                   return (val1, val2, val3)

testReadWritePinIdempotent :: (MonadError e m) => GpioT e h m m (PinValue, PinValue)
testReadWritePinIdempotent =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do void $ setPinDirection d Out
            val1 <- readPin d
            case val1 of
              Low ->
                do void $ writePin d Low
                   val2 <- readPin d
                   closePin d
                   return (val1, val2)
              High ->
                do void $ writePin d High
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

testTogglePinValue :: (MonadError e m) => GpioT e h m m (PinValue, PinValue, PinValue, PinValue, PinValue)
testTogglePinValue =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do void $ setPinDirection d Out
            val1 <- readPin d
            val2 <- togglePinValue d
            val3 <- readPin d
            val4 <- togglePinValue d
            val5 <- readPin d
            closePin d
            return (val1, val2, val3, val4, val5)

invalidHandle :: (MonadError e m) => (h -> GpioT e h m m a) -> GpioT e h m m a
invalidHandle action =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do closePin d
            action d

testWithPin :: (MonadError e m) => GpioT e h m m PinValue
testWithPin = withPin (Pin 1) $ \h ->
  do void $ setPinDirection h Out
     void $ writePin h High
     val <- readPin h
     return val

testWithPinError :: (MonadError e m) => GpioT e h m m PinValue
testWithPinError = withPin (Pin 1) $ \h ->
  do void $ setPinDirection h In
     void $ writePin h High -- should fail
     val <- readPin h
     return val

testNestedWithPin :: (MonadError e m) => GpioT e h m m (PinValue, PinValue)
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

testNestedWithPinError :: (MonadError e m) => GpioT e h m m (PinValue, PinValue)
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

testOpenPinWithValue' :: (MonadError e m) => GpioT e h m m PinValue
testOpenPinWithValue' =
  do handle <- openPinWithValue (Pin 1) Low
     case handle of
       Left e -> throwError e
       Right h ->
         do v <- readPin h
            void $ closePin h
            return v

-- System.IO.Error does not provide this.
myIsInvalidArgumentErrorType :: IOErrorType -> Bool
myIsInvalidArgumentErrorType InvalidArgument = True
myIsInvalidArgumentErrorType _ = False

-- myIsInvalidArgument :: IOError -> Bool
-- myIsInvalidArgument = myIsInvalidArgument . ioe_type

spec :: Spec
spec =
  do describe "pins" $
       let pinList = [Pin 1, Pin 8]
           world = mockWorld pinList
           expectedResult = (pinList, world, [])
       in
         it "returns the list of available pins" $
             runSysfsMock pins world `shouldReturn` expectedResult

     describe "openPin and closePin" $
       let world1 = mockWorld [Pin 1]
           world2 = mockWorld [Pin 2]
           expectedResult1 = ((), world1, [])
       in
         do it "succeeds when the pin is available" $
              runSysfsMock testOpenClose world1 `shouldReturn` expectedResult1
            it "fails when the pin is unavailable" $
              do runSysfsMock testOpenClose world2 `shouldThrow` anyIOException

     describe "setPinDirection" $
       do it "sets the pin direction" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Out, In, Out), world, [])
            in runSysfsMock testSetDirection world `shouldReturn` expectedResult

          it "is idempotent" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Out, Out), world, [])
            in runSysfsMock testSetDirectionIdempotent world `shouldReturn` expectedResult

     describe "togglePinDirection" $
       do it "toggles pin direction" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Out, In, In, Out, Out), world, [])
            in runSysfsMock testTogglePinDirection world `shouldReturn` expectedResult

     describe "writePin" $
       do it "sets the pin value" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Low, High, Low), world, [])
            in runSysfsMock testReadWritePin world `shouldReturn` expectedResult

          it "is idempotent" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Low, Low), world, [])
            in runSysfsMock testReadWritePinIdempotent world `shouldReturn` expectedResult

          it "fails when the pin direction is In" $
            -- Note that this test will leave Pin 1 in the "open" state
            let world = mockWorld [Pin 1]
            in runSysfsMock testWritePinFailsOnInputPin world `shouldThrow` anyIOException

     describe "togglePinValue" $
       do it "toggles the pin's value" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Low, High, High, Low, Low), world, [])
            in runSysfsMock testTogglePinValue world `shouldReturn` expectedResult

     describe "invalid handles throw exceptions" $
       do it "in getPinDirection" $
            let world = mockWorld []
            in runSysfsMock (invalidHandle getPinDirection) world `shouldThrow` anyIOException

          it "in setPinDirection" $
            let world = mockWorld [Pin 1]
            in runSysfsMock (invalidHandle (\d -> setPinDirection d Out)) world `shouldThrow` anyIOException

          it "in readPin" $
            let world = mockWorld [Pin 1]
            in runSysfsMock (invalidHandle readPin) world `shouldThrow` anyIOException

          it "in writePin" $
             let world = mockWorld [Pin 1]
             in runSysfsMock (invalidHandle (\d -> writePin d High)) world `shouldThrow` anyIOException

     describe "withPin" $
       do it "opens and closes the pin as expected" $
            let world = MockWorld (Map.fromList $ zip [Pin 1] [MockState True Out High]) Map.empty
                expectedResult = (High, world, [])
            in runSysfsMock testWithPin (mockWorld [Pin 1]) `shouldReturn` expectedResult

          it "throws an exception when the pin doesn't exist" $
            runSysfsMock testWithPin (mockWorld [Pin 2]) `shouldThrow` anyIOException

          it "can nest" $
            let world = MockWorld {unexported = Map.fromList [(Pin 1,MockState {hasUserDirection = True, direction = Out, value = High}),(Pin 2,MockState {hasUserDirection = True, direction = Out, value = Low})], exported = Map.empty}
                expectedResult = ((High, Low), world, [])
            in runSysfsMock testNestedWithPin world `shouldReturn` expectedResult

          it "fails properly when nested" $
            do runSysfsMock testNestedWithPin (mockWorld [Pin 1]) `shouldThrow` anyIOException
               runSysfsMock testNestedWithPinError (mockWorld [Pin 1, Pin 2]) `shouldThrow` anyIOException

     describe "runSysfsMock'" $
       do it "returns results as Right a" $
            let world = mockWorld [Pin 1]
                expectedResult = (Right (Out, In, Out), world, [])
            in runSysfsMock' testSetDirection world `shouldReturn` expectedResult

          it "returns errors in the computation as Left String" $
            let world = MockWorld {unexported = Map.fromList [(Pin 1, MockState False Out Low)], exported = Map.empty}
                expectedResult = (Left "Can't configure Pin 1 for output", world, [])
            in runSysfsMock' testOpenPinWithValue' world `shouldReturn` expectedResult

          it "returns IO errors as IOException" $
            runSysfsMock' testOpenPinWithValue' (mockWorld []) `shouldThrow` anyIOException

     describe "runSysfsMockSafe" $
       do it "returns results as Right a" $
            let world = mockWorld [Pin 1]
                expectedResult = Right ((Out, In, Out), world, [])
            in runSysfsMockSafe testSetDirection world `shouldReturn` expectedResult

          it "returns errors in the computation as Left String" $
            let world = MockWorld {unexported = Map.fromList [(Pin 1, MockState False Out Low)], exported = Map.empty}
                expectedResult = Left "user error (Can't configure Pin 1 for output)"
            in runSysfsMockSafe testOpenPinWithValue' world `shouldReturn` expectedResult

          it "returns IO errors as Left String" $
            let world = mockWorld []
                expectedResult = Left "/sys/class/gpio/export: hClose: invalid argument (Invalid argument)"
            in runSysfsMockSafe testOpenPinWithValue' world `shouldReturn` expectedResult
