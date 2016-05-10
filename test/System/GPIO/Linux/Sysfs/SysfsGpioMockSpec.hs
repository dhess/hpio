{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.SysfsGpioMockSpec (spec) where

import Control.Exception (fromException)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, handle)
import qualified Data.Map.Strict as Map (lookup)

import System.GPIO.Linux.Sysfs.Types (SysfsEdge(..))
import System.GPIO.Linux.Sysfs.Types (SysfsException(..))
import System.GPIO.Linux.Sysfs.Mock
import System.GPIO.Monad (MonadGpio(..), withPin)
import System.GPIO.Types (Pin (..), PinDirection(..), PinReadTrigger(..), PinValue (..), SomeGpioException)

import Test.Hspec

-- To some extent, these tests only test the implementation of the
-- Mock interpreter, which isn't particularly valuable on its own.
-- However, it does allow us to test that we're able to express the
-- kinds of GpioT programs we want, in a pure environment (and when
-- the platform we're developing on doesn't actually have GPIO
-- functionality).

testOpenClose :: (MonadGpio h m) => m ()
testOpenClose =
  do h <- openPin (Pin 1)
     closePin h

testSetDirection :: (MonadGpio h m) => m (PinDirection, PinDirection, PinDirection)
testSetDirection =
  do d <- openPin (Pin 1)
     (Just dir1) <- getPinDirection d
     case dir1 of
       In ->
         do setPinDirection d Out
            (Just dir2) <- getPinDirection d
            setPinDirection d In
            (Just dir3) <- getPinDirection d
            closePin d
            return (dir1,dir2,dir3)
       Out ->
         do setPinDirection d In
            (Just dir2) <- getPinDirection d
            setPinDirection d Out
            (Just dir3) <- getPinDirection d
            closePin d
            return (dir1,dir2,dir3)

testSetReadTrigger :: (MonadGpio h m) => m (Maybe PinReadTrigger, Maybe PinReadTrigger, Maybe PinReadTrigger, Maybe PinReadTrigger)
testSetReadTrigger =
  do d <- openPin (Pin 1)
     setPinDirection d In
     setPinReadTrigger d Disabled
     t1 <- getPinReadTrigger d
     setPinReadTrigger d RisingEdge
     t2 <- getPinReadTrigger d
     setPinReadTrigger d FallingEdge
     t3 <- getPinReadTrigger d
     setPinReadTrigger d Level
     t4 <- getPinReadTrigger d
     closePin d
     return (t1, t2, t3, t4)

testSetReadTriggerIdempotent :: (MonadGpio h m) => m (Maybe PinReadTrigger, Maybe PinReadTrigger)
testSetReadTriggerIdempotent =
  do d <- openPin (Pin 1)
     setPinDirection d In
     setPinReadTrigger d FallingEdge
     t1 <- getPinReadTrigger d
     setPinReadTrigger d FallingEdge
     t2 <- getPinReadTrigger d
     closePin d
     return (t1, t2)

testSetDirectionIdempotent :: (MonadGpio h m) => m (PinDirection, PinDirection)
testSetDirectionIdempotent =
  do d <- openPin (Pin 1)
     (Just dir1) <- getPinDirection d
     case dir1 of
       In ->
         do setPinDirection d In
            (Just dir2) <- getPinDirection d
            closePin d
            return (dir1,dir2)
       Out ->
         do setPinDirection d Out
            (Just dir2) <- getPinDirection d
            closePin d
            return (dir1,dir2)

testTogglePinDirection :: (MonadGpio h m) => m (PinDirection, PinDirection, PinDirection, PinDirection, PinDirection)
testTogglePinDirection =
  do d <- openPin (Pin 1)
     (Just dir1) <- getPinDirection d
     (Just dir2) <- togglePinDirection d
     (Just dir3) <- getPinDirection d
     (Just dir4) <- togglePinDirection d
     (Just dir5) <- getPinDirection d
     closePin d
     return (dir1, dir2, dir3, dir4, dir5)

testSampleWritePin :: (MonadGpio h m) => h -> m (PinValue, PinValue, PinValue)
testSampleWritePin d =
  do val1 <- samplePin d
     case val1 of
       Low ->
         do writePin d High
            val2 <- samplePin d
            writePin d Low
            val3 <- samplePin d
            return (val1,val2,val3)
       High ->
         do writePin d Low
            val2 <- samplePin d
            writePin d High
            val3 <- samplePin d
            return (val1,val2,val3)

testSampleWritePinIdempotent :: (MonadGpio h m) => m (PinValue, PinValue)
testSampleWritePinIdempotent =
  do d <- openPin (Pin 1)
     setPinDirection d Out
     val1 <- samplePin d
     case val1 of
       Low ->
         do writePin d Low
            val2 <- samplePin d
            closePin d
            return (val1,val2)
       High ->
         do writePin d High
            val2 <- samplePin d
            closePin d
            return (val1,val2)

testWritePinFailsOnInputPin :: (MonadGpio h m) => m ()
testWritePinFailsOnInputPin =
  do d <- openPin (Pin 1)
     setPinDirection d In
     writePin d High
     closePin d

testWritePin' :: (MonadGpio h m) => m (PinValue, PinValue, Maybe PinDirection, Maybe PinDirection)
testWritePin' =
  do d1 <- openPin (Pin 1)
     d2 <- openPin (Pin 2)
     writePin' d1 High
     val1 <- samplePin d1
     writePin' d2 Low
     val2 <- samplePin d2
     dir1 <- getPinDirection d1
     dir2 <- getPinDirection d2
     closePin d1
     closePin d2
     return (val1, val2, dir1, dir2)

testWritePinIdempotent' :: (MonadGpio h m) => m (PinValue, PinValue, PinValue, PinValue)
testWritePinIdempotent' =
  do d <- openPin (Pin 1)
     writePin' d High
     val1 <- samplePin d
     writePin' d High
     val2 <- samplePin d
     writePin' d Low
     val3 <- samplePin d
     writePin' d Low
     val4 <- samplePin d
     closePin d
     return (val1, val2, val3, val4)

testTogglePinValue :: (MonadGpio h m) => m (PinValue, PinValue, PinValue, PinValue, PinValue)
testTogglePinValue =
  do d <- openPin (Pin 1)
     setPinDirection d Out
     val1 <- samplePin d
     val2 <- togglePinValue d
     val3 <- samplePin d
     val4 <- togglePinValue d
     val5 <- samplePin d
     closePin d
     return (val1, val2, val3, val4, val5)

testTogglePinActiveLevel :: (MonadGpio h m) => h -> m (PinValue, PinValue, PinValue, PinValue, PinValue)
testTogglePinActiveLevel h =
  do val1 <- getPinActiveLevel h
     val2 <- togglePinActiveLevel h
     val3 <- getPinActiveLevel h
     val4 <- togglePinActiveLevel h
     val5 <- getPinActiveLevel h
     return (val1, val2, val3, val4, val5)

invalidHandle :: (MonadGpio h m) => (h -> m a) -> m a
invalidHandle action =
  do d <- openPin (Pin 1)
     closePin d
     action d

testWithPin :: (MonadGpio h m, MonadMask m) => m PinValue
testWithPin = withPin (Pin 1) $ \h ->
  do setPinDirection h Out
     writePin h High
     val <- samplePin h
     return val

testNestedWithPin :: (MonadGpio h m, MonadMask m) => Pin -> Pin -> m (PinValue, PinValue)
testNestedWithPin p1 p2 =
  withPin p1 $ \h1 ->
    withPin p2 $ \h2 ->
      do setPinDirection h1 Out
         setPinDirection h2 Out
         writePin h1 High
         writePin h2 Low
         val1 <- samplePin h1
         val2 <- samplePin h2
         return (val1, val2)

handleGpioException :: MonadCatch m => (SomeGpioException -> m a) -> m a -> m a
handleGpioException = handle

testWithPinError :: (MonadGpio h m, MonadMask m, MonadCatch m) => Pin -> Pin -> m (Maybe PinValue)
testWithPinError p1 p2 = handleGpioException (const $ return Nothing) $
  withPin p1 $ \h1 ->
    withPin p2 $ \h2 ->
      do setPinDirection h1 Out
         setPinDirection h2 In
         writePin h1 High -- should fail if p1 == p2
         writePin h2 Low -- should fail in any case
         val1 <- samplePin h1
         return $ Just val1

chip0 :: MockGpioChip
chip0 = MockGpioChip "chip0" 0 (replicate 16 defaultMockPinState)

chip1 :: MockGpioChip
chip1 = MockGpioChip "chip1" 32 (replicate 32 defaultMockPinState)

evalSysfsGpioMock' :: SysfsGpioMock a -> MockWorld -> [MockGpioChip] -> Either (Maybe SysfsException) a
evalSysfsGpioMock' a w c = either (Left . fromException) return $ evalSysfsGpioMock a w c

execSysfsGpioMock' :: SysfsGpioMock a -> MockWorld -> [MockGpioChip] -> Either (Maybe SysfsException) MockWorld
execSysfsGpioMock' a w c = either (Left . fromException) return $ execSysfsGpioMock a w c

runSysfsGpioMock' :: SysfsGpioMock a -> MockWorld -> [MockGpioChip] -> Either (Maybe SysfsException) (a, MockWorld)
runSysfsGpioMock' a w c = either (Left . fromException) return $ runSysfsGpioMock a w c

evalSysfsMock' :: SysfsMock a -> MockWorld -> [MockGpioChip] -> Either (Maybe SysfsException) a
evalSysfsMock' a w c = either (Left . fromException) Right $ evalSysfsMock a w c

spec :: Spec
spec =
  do describe "pins" $
       let pinList = map Pin [0..15]
           expectedResult = Right pinList
       in
         it "returns the list of available pins" $
             evalSysfsGpioMock' pins initialMockWorld [chip0] `shouldBe` expectedResult

     describe "openPin/closePin" $
       do it "cleans up properly" $
            let (Right world) = execSysfsGpioMock' testOpenClose initialMockWorld [chip0]
            in evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio1") world [] `shouldBe` Right False
          it "fails when the pin is unavailable" $
            evalSysfsGpioMock' testOpenClose initialMockWorld [chip1] `shouldBe` Left (Just $ InvalidPin (Pin 1))
          it "openPin does not error when the pin is already open" $
            evalSysfsGpioMock' (openPin (Pin 1) >> openPin (Pin 1) >> return ()) initialMockWorld [chip0] `shouldBe` Right ()

          it "closePin does not error when the pin is already closed" $
            evalSysfsGpioMock' (do { h <- openPin (Pin 1) ; closePin h ; closePin h} ) initialMockWorld [chip0] `shouldBe` Right ()

     describe "getPinDirection" $
       do it "gets the pin's direction" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In}]
            in
              do evalSysfsGpioMock' (withPin (Pin 1) getPinDirection) initialMockWorld [chip0] `shouldBe` Right (Just Out)
                 evalSysfsGpioMock' (withPin (Pin 1) getPinDirection) initialMockWorld [testChip] `shouldBe` Right (Just In)

          it "returns Nothing when the pin direction is not settable" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_userVisibleDirection = False}]
            in evalSysfsGpioMock' (withPin (Pin 1) getPinDirection) initialMockWorld [testChip] `shouldBe` Right Nothing

     describe "setPinDirection" $
       do it "sets the pin direction" $
            let expectedResult = Right (Out, In, Out)
            in evalSysfsGpioMock' testSetDirection initialMockWorld [chip0] `shouldBe` expectedResult

          it "can change the pin direction to 'Out' when the pin is configured for edge- or level-triggered reads " $
            evalSysfsGpioMock'
              (withPin (Pin 1) $ \h ->
                do setPinDirection h In
                   setPinReadTrigger h RisingEdge
                   setPinDirection h Out
                   setPinDirection h In
                   setPinReadTrigger h FallingEdge
                   setPinDirection h Out
                   setPinDirection h In
                   setPinReadTrigger h Level
                   setPinDirection h Out
                   return True)
              initialMockWorld
              [chip0]
            `shouldBe` Right True
          it "is idempotent" $
            evalSysfsGpioMock' testSetDirectionIdempotent initialMockWorld [chip0] `shouldBe` Right (Out, Out)

          it "fails when the pin's direction is not settable" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_userVisibleDirection = False}]
            in evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinDirection h Out)) initialMockWorld [testChip] `shouldBe` Left (Just $ NoDirectionAttribute (Pin 1))

     describe "togglePinDirection" $
       do it "toggles pin direction" $
            evalSysfsGpioMock' testTogglePinDirection initialMockWorld [chip0] `shouldBe` Right (Out, In, In, Out, Out)
          it "toggles the pin's direction when the pin is configured for input and edge- or level-triggered reads" $
            evalSysfsGpioMock'
              (withPin (Pin 1) $ \h ->
                do setPinDirection h In
                   setPinReadTrigger h RisingEdge
                   void $ togglePinDirection h
                   setPinDirection h In
                   setPinReadTrigger h FallingEdge
                   void $ togglePinDirection h
                   setPinDirection h In
                   setPinReadTrigger h Level
                   void $ togglePinDirection h
                   return True)
              initialMockWorld
              [chip0]
            `shouldBe` Right True
          it "returns Nothing when the pin direction is not settable" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_userVisibleDirection = False}]
            in evalSysfsGpioMock' (withPin (Pin 1) togglePinDirection) initialMockWorld [testChip] `shouldBe` Right Nothing

     describe "getPinReadTrigger" $
       do it "gets the pin's read trigger" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_edge = Just Falling}]
            in
              do evalSysfsGpioMock' (withPin (Pin 1) getPinReadTrigger) initialMockWorld [chip0] `shouldBe` Right (Just Disabled)
                 evalSysfsGpioMock' (withPin (Pin 1) getPinReadTrigger) initialMockWorld [testChip] `shouldBe` Right (Just FallingEdge)

          it "returns Nothing when the pin's read trigger is not settable" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_edge = Nothing}]
            in evalSysfsGpioMock' (withPin (Pin 1) getPinReadTrigger) initialMockWorld [testChip] `shouldBe` Right Nothing

     describe "setPinReadTrigger" $
       do it "sets the pin's read trigger" $
            evalSysfsGpioMock' testSetReadTrigger initialMockWorld [chip0] `shouldBe` Right (Just Disabled, Just RisingEdge, Just FallingEdge, Just Level)
          it "is idempotent" $
            evalSysfsGpioMock' testSetReadTriggerIdempotent initialMockWorld [chip0] `shouldBe` Right (Just FallingEdge, Just FallingEdge)
          it "fails when the pin's read trigger is not settable" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_edge = Nothing}, defaultMockPinState]
            in evalSysfsGpioMock' testSetReadTrigger initialMockWorld [testChip] `shouldBe` Left (Just $ NoEdgeAttribute (Pin 1))
          it "fails when the pin is configured for output" $
            evalSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinReadTrigger h Level) initialMockWorld [chip0] `shouldBe` Left (Just $ InvalidOperation (Pin 1))

     describe "getPinActiveLevel" $
       do context "when active level is high" $
            do it "returns the pin's active level" $
                   evalSysfsGpioMock' (withPin (Pin 1) getPinActiveLevel) initialMockWorld [chip0] `shouldBe` Right High
          context "when active level is low" $
            do it "returns the pin's active level" $
                 let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_activeLow = True}]
                 in evalSysfsGpioMock' (withPin (Pin 1) getPinActiveLevel) initialMockWorld [testChip] `shouldBe` Right Low

     describe "setPinActiveLevel" $
       do context "when active level is high" $
            do it "sets the pin's active level to low" $
                 let (Right world) = execSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h Low)) initialMockWorld [chip0]
                 in Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = True})
               it "sets the pin's active level to high" $
                 let (Right world) = execSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h High)) initialMockWorld [chip0]
                 in Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = False})
          context "when active level is low" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_activeLow = True}]
            in do it "sets the pin's active level to low" $
                    let (Right world) = execSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h Low)) initialMockWorld [testChip]
                    in Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = True})
                  it "sets the pin's active level to high" $
                    let (Right world) = execSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h High)) initialMockWorld [chip0]
                    in Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = False})

     describe "togglePinActiveLevel" $
       do it "toggles the pin's active level when the pin is configured for output" $
            evalSysfsGpioMock' (withPin (Pin 1) testTogglePinActiveLevel) initialMockWorld [chip0] `shouldBe` Right (High, Low, Low, High, High)
          it "and when the pin is configured for input" $
            evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinDirection h In >> testTogglePinActiveLevel h)) initialMockWorld [chip0] `shouldBe` Right (High, Low, Low, High, High)

     describe "readPin" $
       do it "waits for the specified trigger and returns the pin's value" $
            pendingWith "need to implement this"

          it "blocks when the read trigger is Disabled, until it is changed" $
            pendingWith "need to implement this"

     describe "readPinTimeout" $
       do it "waits for the specified trigger and returns the pin's value" $
            pendingWith "need to implement this"
          it "blocks when the read trigger is Disabled, until it is changed" $
            pendingWith "need to implement this"
          it "times out" $
            pendingWith "need to implement this"

     describe "samplePin" $
       do context "returns the pin's logical level" $
            do it "when the active level is high" $
                 evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h High >> samplePin h)) initialMockWorld [chip0] `shouldBe` Right Low
               it "when the active level is low" $
                 evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h Low >> samplePin h)) initialMockWorld [chip0] `shouldBe` Right High

     describe "writePin" $
       do context "sets the pin's logical value" $
            do it "when the active level is high" $
                 let Right (result, world) = runSysfsGpioMock' (withPin (Pin 1) testSampleWritePin) initialMockWorld [chip0]
                 in do result `shouldBe` (Low, High, Low)
                       Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState
               it "when the active level is low" $
                 let Right (result, world) = runSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h Low >> testSampleWritePin h)) initialMockWorld [chip0]
                 in do result `shouldBe` (High, Low, High)
                       Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = True, _value = Low})

          it "is idempotent" $
            evalSysfsGpioMock' testSampleWritePinIdempotent initialMockWorld [chip0] `shouldBe` Right (Low, Low)

          it "fails when the pin direction is In" $
            evalSysfsGpioMock' testWritePinFailsOnInputPin initialMockWorld [chip0] `shouldBe` Left (Just $ PermissionDenied (Pin 1))

     describe "writePin'" $
       do context "sets the pin's logical value and direction" $
            do it "when the active level is high" $
                 let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In}, defaultMockPinState {_value = High}]
                     Right (result, world) = runSysfsGpioMock' testWritePin' initialMockWorld [testChip]
                 in do result `shouldBe` (High, Low, Just Out, Just Out)
                       Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_value = High})
                       Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState

               it "when the active level is low" $
                 let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In, _activeLow = True}, defaultMockPinState {_activeLow = True, _value = High}]
                     Right (result, world) = runSysfsGpioMock' testWritePin' initialMockWorld [testChip]
                 in do result `shouldBe` (High, Low, Just Out, Just Out)
                       Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = True})
                       Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = True, _value = High})

          it "works when the pin is configured for input and edge- or level-triggered reads" $
            evalSysfsGpioMock'
              (withPin (Pin 1) $ \h ->
                do setPinDirection h In
                   setPinReadTrigger h RisingEdge
                   writePin' h High
                   val1 <- samplePin h
                   setPinDirection h In
                   setPinReadTrigger h RisingEdge
                   writePin' h High
                   val2 <- samplePin h
                   setPinDirection h In
                   setPinReadTrigger h RisingEdge
                   writePin' h High
                   val3 <- samplePin h
                   return (val1, val2, val3))
              initialMockWorld
              [chip0]
            `shouldBe` Right (High, High, High)
          it "is idempotent" $
            evalSysfsGpioMock' testWritePinIdempotent' initialMockWorld [chip0] `shouldBe` Right (High, High, Low, Low)

          it "fails when the pin's direction is not settable" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_userVisibleDirection = False, _value = High}, defaultMockPinState]
            in evalSysfsGpioMock' testWritePin' initialMockWorld [testChip] `shouldBe` Left (Just $ NoDirectionAttribute (Pin 1))

     describe "togglePinValue" $
       do it "toggles the pin's value" $
            evalSysfsGpioMock' testTogglePinValue initialMockWorld [chip0] `shouldBe` Right (Low, High, High, Low, Low)
          it "fails when the pin is not configured for output" $
            evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinDirection h In >> togglePinValue h)) initialMockWorld [chip0] `shouldBe` Left (Just $ PermissionDenied (Pin 1))

     describe "operations on an invalid handle fail" $
       -- Note: When used on an invalid handle, GPIO commands which
       -- return a 'Maybe' result will fail differently than commands
       -- which do not. This is by design.
       do it "in getPinDirection" $
             evalSysfsGpioMock' (invalidHandle getPinDirection) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in setPinDirection" $
             evalSysfsGpioMock' (invalidHandle (\h -> setPinDirection h Out)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in togglePinDirection" $
            evalSysfsGpioMock' (invalidHandle togglePinDirection) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in setPinDirection" $
            evalSysfsGpioMock' (invalidHandle (\d -> setPinDirection d Out)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in samplePin" $
            evalSysfsGpioMock' (invalidHandle samplePin) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in readPin" $
            evalSysfsGpioMock' (invalidHandle readPin) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in readPinTimeout" $
            evalSysfsGpioMock' (invalidHandle (\h -> readPinTimeout h 10000)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in writePin" $
             evalSysfsGpioMock' (invalidHandle (\d -> writePin d High)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in writePin'" $
             evalSysfsGpioMock' (invalidHandle (\d -> writePin' d High)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in togglePinValue" $
            evalSysfsGpioMock' (invalidHandle togglePinValue) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in getPinReadTrigger" $
            evalSysfsGpioMock' (invalidHandle getPinReadTrigger) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in setPinReadTrigger" $
             evalSysfsGpioMock' (invalidHandle (\d -> setPinReadTrigger d Level)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in getPinActiveLevel" $
            evalSysfsGpioMock' (invalidHandle getPinActiveLevel) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in setPinActiveLevel" $
             evalSysfsGpioMock' (invalidHandle (\d -> setPinActiveLevel d High)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in togglePinActiveLevel" $
             evalSysfsGpioMock' (invalidHandle togglePinActiveLevel) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

     describe "withPin" $
       do it "opens and closes the pin as expected" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_value = High}]
                (Right world) = execSysfsGpioMock' testWithPin initialMockWorld [testChip]
            in evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio1") world [] `shouldBe` Right False

          it "throws an exception when the pin doesn't exist" $
            evalSysfsGpioMock' testWithPin initialMockWorld [chip1] `shouldBe` Left (Just $ InvalidPin (Pin 1))

          it "can nest" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_value = High}, defaultMockPinState]
                (Right (result, world)) = runSysfsGpioMock' (testNestedWithPin (Pin 1) (Pin 2)) initialMockWorld [testChip]
            in do result `shouldBe` (High, Low)
                  evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio1") world [] `shouldBe` Right False
                  evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio2") world [] `shouldBe` Right False

          it "handles double-open and double-close without complaint" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_value = High}, defaultMockPinState]
                (Right (result, world)) = runSysfsGpioMock' (testNestedWithPin (Pin 1) (Pin 1)) initialMockWorld [testChip]
            in do result `shouldBe` (Low, Low)
                  evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio1") world [] `shouldBe` Right False

          it "cleans up when an exception occurs in the computation" $
            let (Right (result, world)) = runSysfsGpioMock' (testWithPinError (Pin 1) (Pin 2)) initialMockWorld [chip0]
            in do result `shouldBe` Nothing
                  evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio1") world [] `shouldBe` Right False
                  evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio2") world [] `shouldBe` Right False
