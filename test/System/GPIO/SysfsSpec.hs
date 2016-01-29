{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.SysfsSpec (spec) where

import Control.Monad.Except
import qualified Data.Map.Strict as Map
import System.GPIO.Free
import System.GPIO.Linux.SysfsMock
import System.GPIO.Types

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
                do setPinDirection d Out
                   (Just dir2) <- getPinDirection d
                   setPinDirection d In
                   (Just dir3) <- getPinDirection d
                   closePin d
                   return (dir1, dir2, dir3)
              Out ->
                do setPinDirection d In
                   (Just dir2) <- getPinDirection d
                   setPinDirection d Out
                   (Just dir3) <- getPinDirection d
                   closePin d
                   return (dir1, dir2, dir3)

testSetReadTrigger :: (MonadError e m) => GpioT e h m m (Maybe PinReadTrigger, Maybe PinReadTrigger, Maybe PinReadTrigger, Maybe PinReadTrigger)
testSetReadTrigger =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do setPinReadTrigger d Disabled
            t1 <- getPinReadTrigger d
            setPinReadTrigger d RisingEdge
            t2 <- getPinReadTrigger d
            setPinReadTrigger d FallingEdge
            t3 <- getPinReadTrigger d
            setPinReadTrigger d Level
            t4 <- getPinReadTrigger d
            closePin d
            return (t1, t2, t3, t4)

testSetReadTriggerIdempotent :: (MonadError e m) => GpioT e h m m (Maybe PinReadTrigger, Maybe PinReadTrigger)
testSetReadTriggerIdempotent =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do setPinReadTrigger d FallingEdge
            t1 <- getPinReadTrigger d
            setPinReadTrigger d FallingEdge
            t2 <- getPinReadTrigger d
            closePin d
            return (t1, t2)

testSetDirectionIdempotent :: (MonadError e m) => GpioT e h m m (PinDirection, PinDirection)
testSetDirectionIdempotent =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do (Just dir1) <- getPinDirection d
            case dir1 of
              In ->
                do setPinDirection d In
                   (Just dir2) <- getPinDirection d
                   closePin d
                   return (dir1, dir2)
              Out ->
                do setPinDirection d Out
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

testSampleWritePin :: (MonadError e m) => GpioT e h m m (PinValue, PinValue, PinValue)
testSampleWritePin =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do setPinDirection d Out
            val1 <- samplePin d
            case val1 of
              Low ->
                do writePin d High
                   val2 <- samplePin d
                   writePin d Low
                   val3 <- samplePin d
                   closePin d
                   return (val1, val2, val3)
              High ->
                do writePin d Low
                   val2 <- samplePin d
                   writePin d High
                   val3 <- samplePin d
                   closePin d
                   return (val1, val2, val3)

testSampleWritePinIdempotent :: (MonadError e m) => GpioT e h m m (PinValue, PinValue)
testSampleWritePinIdempotent =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do setPinDirection d Out
            val1 <- samplePin d
            case val1 of
              Low ->
                do writePin d Low
                   val2 <- samplePin d
                   closePin d
                   return (val1, val2)
              High ->
                do writePin d High
                   val2 <- samplePin d
                   closePin d
                   return (val1, val2)

testWritePinFailsOnInputPin :: (MonadError e m) => GpioT e h m m ()
testWritePinFailsOnInputPin =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do setPinDirection d In
            writePin d High
            closePin d

testWritePin' :: (MonadError e m) => GpioT e h m m (PinValue, PinValue, Maybe PinDirection, Maybe PinDirection)
testWritePin' =
  do handle1 <- openPin (Pin 1)
     handle2 <- openPin (Pin 2)
     case (handle1, handle2) of
       (Left e, _) -> throwError e
       (_, Left e) -> throwError e
       (Right d1, Right d2) ->
         do writePin' d1 High
            val1 <- samplePin d1
            writePin' d2 Low
            val2 <- samplePin d2
            dir1 <- getPinDirection d1
            dir2 <- getPinDirection d2
            closePin d1
            closePin d2
            return (val1, val2, dir1, dir2)

testWritePinIdempotent' :: (MonadError e m) => GpioT e h m m (PinValue, PinValue, PinValue, PinValue)
testWritePinIdempotent' =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do writePin' d High
            val1 <- samplePin d
            writePin' d High
            val2 <- samplePin d
            writePin' d Low
            val3 <- samplePin d
            writePin' d Low
            val4 <- samplePin d
            closePin d
            return (val1, val2, val3, val4)

testTogglePinValue :: (MonadError e m) => GpioT e h m m (PinValue, PinValue, PinValue, PinValue, PinValue)
testTogglePinValue =
  do handle <- openPin (Pin 1)
     case handle of
       Left e -> throwError e
       Right d ->
         do setPinDirection d Out
            val1 <- samplePin d
            val2 <- togglePinValue d
            val3 <- samplePin d
            val4 <- togglePinValue d
            val5 <- samplePin d
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
  do setPinDirection h Out
     writePin h High
     val <- samplePin h
     return val

testNestedWithPin :: (MonadError e m) => GpioT e h m m (PinValue, PinValue)
testNestedWithPin =
  withPin (Pin 1) $ \h1 ->
    withPin (Pin 2) $ \h2 ->
      do setPinDirection h1 Out
         setPinDirection h2 Out
         writePin h1 High
         writePin h2 Low
         val1 <- samplePin h1
         val2 <- samplePin h2
         return (val1, val2)

testNestedWithPinError :: (MonadError e m) => GpioT e h m m (PinValue, PinValue)
testNestedWithPinError =
  withPin (Pin 1) $ \h1 ->
    withPin (Pin 2) $ \h2 ->
      do setPinDirection h1 Out
         setPinDirection h2 In
         writePin h1 High
         writePin h2 Low -- should fail
         val1 <- samplePin h1
         val2 <- samplePin h2
         return (val1, val2)

testErrorInComputation :: (MonadError String m) => GpioT e h m m h
testErrorInComputation =
  do handle <- openPin (Pin 1)
     case handle of
       Left _ -> throwError "openPin failed"
       Right h ->
         do closePin h
            throwError "Expected error"

spec :: Spec
spec =
  do describe "pins" $
       let pinList = [Pin 1, Pin 8]
           world = mockWorld pinList
           expectedResult = (pinList, world)
       in
         it "returns the list of available pins" $
             runSysfsMock pins world `shouldReturn` expectedResult

     describe "openPin and closePin" $
       let world1 = mockWorld [Pin 1]
           world2 = mockWorld [Pin 2]
           expectedResult1 = ((), world1)
       in
         do it "succeeds when the pin is available" $
              runSysfsMock testOpenClose world1 `shouldReturn` expectedResult1
            it "fails when the pin is unavailable" $
              do runSysfsMock testOpenClose world2 `shouldThrow` anyIOException

     describe "getPinDirection" $
       do it "gets the pin's direction" $
            let world1 = mockWorld [Pin 1]
                world2 =
                  Map.fromList [(Pin 1, defaultState {direction = In})]
                expectedResult1 = (Just Out, world1)
                expectedResult2 = (Just In, world2)
            in
              do runSysfsMock (withPin (Pin 1) (\h -> getPinDirection h >>= return)) world1 `shouldReturn` expectedResult1
                 runSysfsMock (withPin (Pin 1) (\h -> getPinDirection h >>= return)) world2 `shouldReturn` expectedResult2

          it "returns Nothing when the pin direction is not settable" $
            let world =
                  Map.fromList [(Pin 1, defaultState {hasUserDirection = False})]
                expectedResult = (Nothing, world)
            in runSysfsMock (withPin (Pin 1) (\h -> getPinDirection h >>= return)) world `shouldReturn` expectedResult

     describe "setPinDirection" $
       do it "sets the pin direction" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Out, In, Out), world)
            in runSysfsMock testSetDirection world `shouldReturn` expectedResult

          it "is idempotent" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Out, Out), world)
            in runSysfsMock testSetDirectionIdempotent world `shouldReturn` expectedResult

          it "fails when the pin's direction is not settable" $
            let world =
                  Map.fromList [(Pin 1, defaultState {hasUserDirection = False, value = High})
                               ,(Pin 2, defaultState)]
            in runSysfsMock testSetDirection world `shouldThrow` anyIOException

     describe "togglePinDirection" $
       do it "toggles pin direction" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Out, In, In, Out, Out), world)
            in runSysfsMock testTogglePinDirection world `shouldReturn` expectedResult

     describe "getPinReadTrigger" $
       do it "gets the pin's read trigger" $
            let world1 = mockWorld [Pin 1]
                world2 =
                  Map.fromList [(Pin 1, defaultState {edge = Just FallingEdge})]
                expectedResult1 = (Just Disabled, world1)
                expectedResult2 = (Just FallingEdge, world2)
            in
              do runSysfsMock (withPin (Pin 1) (\h -> getPinReadTrigger h >>= return)) world1 `shouldReturn` expectedResult1
                 runSysfsMock (withPin (Pin 1) (\h -> getPinReadTrigger h >>= return)) world2 `shouldReturn` expectedResult2
          it "returns Nothing when the pin's read trigger is not settable" $
            let world =
                  Map.fromList [(Pin 1, defaultState {edge = Nothing})]
                expectedResult = (Nothing, world)
            in runSysfsMock (withPin (Pin 1) (\h -> getPinReadTrigger h >>= return)) world `shouldReturn` expectedResult

     describe "setPinReadTrigger" $
       do it "sets the pin's read trigger" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Just Disabled, Just RisingEdge, Just FallingEdge, Just Level),
                                  Map.fromList [(Pin 1, defaultState {edge = Just Level})])
            in runSysfsMock testSetReadTrigger world `shouldReturn` expectedResult
          it "is idempotent" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Just FallingEdge, Just FallingEdge),
                                 Map.fromList [(Pin 1, defaultState {edge = Just FallingEdge})])
            in runSysfsMock testSetReadTriggerIdempotent world `shouldReturn` expectedResult
          it "fails when the pin's direction is not settable" $
            let world =
                  Map.fromList [(Pin 1, defaultState {edge = Nothing})
                               ,(Pin 2, defaultState)]
            in runSysfsMock testSetReadTrigger world `shouldThrow` anyIOException

     describe "readPin" $
       do it "waits for the specified trigger and returns the pin's value" $
            pendingWith "need to implement this in SysfsMock"

          it "blocks when the read trigger is Disabled, until it is changed" $
            pendingWith "need to implement this in SysfsMock"

     describe "writePin" $
       do it "sets the pin value" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Low, High, Low), world)
            in runSysfsMock testSampleWritePin world `shouldReturn` expectedResult

          it "is idempotent" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Low, Low), world)
            in runSysfsMock testSampleWritePinIdempotent world `shouldReturn` expectedResult

          it "fails when the pin direction is In" $
            -- Note that this test will leave Pin 1 in the "open" state
            let world = mockWorld [Pin 1]
            in runSysfsMock testWritePinFailsOnInputPin world `shouldThrow` anyIOException

     describe "writePin'" $
       do it "sets the pin value and direction" $
            let world =
                  Map.fromList [(Pin 1, defaultState {direction = In})
                               ,(Pin 2, defaultState {value = High})]
                expectedResult =
                  ((High, Low, Just Out, Just Out),
                   Map.fromList [(Pin 1, defaultState {value = High})
                                ,(Pin 2, defaultState)])
            in runSysfsMock testWritePin' world `shouldReturn` expectedResult

          it "is idempotent" $
            let world = mockWorld [Pin 1]
                expectedResult = ((High, High, Low, Low), world)
            in runSysfsMock testWritePinIdempotent' world `shouldReturn` expectedResult

          it "fails when the pin's direction is not settable" $
            let world =
                  Map.fromList [(Pin 1, defaultState {hasUserDirection = False, value = High})
                               ,(Pin 2, defaultState)]
            in runSysfsMock testWritePin' world `shouldThrow` anyIOException

     describe "togglePinValue" $
       do it "toggles the pin's value" $
            let world = mockWorld [Pin 1]
                expectedResult = ((Low, High, High, Low, Low), world)
            in runSysfsMock testTogglePinValue world `shouldReturn` expectedResult

     describe "invalid handles throw exceptions" $
       do it "in getPinDirection" $
            let world = mockWorld []
            in runSysfsMock (invalidHandle getPinDirection) world `shouldThrow` anyIOException

          it "in setPinDirection" $
            let world = mockWorld [Pin 1]
            in runSysfsMock (invalidHandle (\d -> setPinDirection d Out)) world `shouldThrow` anyIOException

          it "in samplePin" $
            let world = mockWorld [Pin 1]
            in runSysfsMock (invalidHandle samplePin) world `shouldThrow` anyIOException

          it "in writePin" $
             let world = mockWorld [Pin 1]
             in runSysfsMock (invalidHandle (\d -> writePin d High)) world `shouldThrow` anyIOException

     describe "withPin" $
       do it "opens and closes the pin as expected" $
            let world =
                  Map.fromList [(Pin 1, defaultState {value = High})]
                expectedResult = (High, world)
            in runSysfsMock testWithPin (mockWorld [Pin 1]) `shouldReturn` expectedResult

          it "throws an exception when the pin doesn't exist" $
            runSysfsMock testWithPin (mockWorld [Pin 2]) `shouldThrow` anyIOException

          it "can nest" $
            let world =
                   Map.fromList [(Pin 1, defaultState {value = High})
                                ,(Pin 2, defaultState)]
                expectedResult = ((High, Low), world)
            in runSysfsMock testNestedWithPin world `shouldReturn` expectedResult

          it "fails properly when nested" $
            do runSysfsMock testNestedWithPin (mockWorld [Pin 1]) `shouldThrow` anyIOException
               runSysfsMock testNestedWithPinError (mockWorld [Pin 1, Pin 2]) `shouldThrow` anyIOException

     describe "runSysfsMock'" $
       do it "returns results as Right a" $
            let world = mockWorld [Pin 1]
                expectedResult = (Right (Out, In, Out), world)
            in runSysfsMock' testSetDirection world `shouldReturn` expectedResult

          it "returns errors in the computation as Left String" $
            let world = mockWorld [Pin 1]
                expectedResult = (Left "Expected error", world)
            in runSysfsMock' testErrorInComputation world `shouldReturn` expectedResult

          it "returns IO errors as IOException" $
            runSysfsMock' testOpenClose (mockWorld []) `shouldThrow` anyIOException

          it "works with withPin on success" $
            let world =
                  Map.fromList [(Pin 1, defaultState {value = High})]
                expectedResult = (Right High, world)
            in runSysfsMock' testWithPin (mockWorld [Pin 1]) `shouldReturn` expectedResult

          it "works with withPin on failure" $
             runSysfsMock testWithPin (mockWorld [Pin 2]) `shouldThrow` anyIOException

     describe "runSysfsMockSafe" $
       do it "returns results as Right a" $
            let world = mockWorld [Pin 1]
                expectedResult = Right ((Out, In, Out), world)
            in runSysfsMockSafe testSetDirection world `shouldReturn` expectedResult

          it "returns errors in the computation as Left String" $
            let world = mockWorld [Pin 1]
                expectedResult = Left "user error (Expected error)"
            in runSysfsMockSafe testErrorInComputation world `shouldReturn` expectedResult

          it "returns IO errors as Left String" $
            let world = mockWorld []
                expectedResult = Left "/sys/class/gpio/export: hClose: invalid argument (Invalid argument)"
            in runSysfsMockSafe testOpenClose world `shouldReturn` expectedResult

          it "works with withPin on success" $
            let world =
                  Map.fromList [(Pin 1, defaultState {value = High})]
                expectedResult = Right (High, world)
            in runSysfsMockSafe testWithPin (mockWorld [Pin 1]) `shouldReturn` expectedResult

          it "works with withPin on failure" $
            runSysfsMockSafe testWithPin (mockWorld [Pin 2]) `shouldReturn` Left "/sys/class/gpio/export: hClose: invalid argument (Invalid argument)"
