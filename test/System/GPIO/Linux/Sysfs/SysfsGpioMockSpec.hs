{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.SysfsGpioMockSpec (spec) where

import Control.Exception (fromException)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, handle)
import qualified Data.Map.Strict as Map (lookup)

import System.GPIO.Linux.Sysfs.Mock
import System.GPIO.Linux.Sysfs.Types (SysfsEdge(..), SysfsException(..))
import System.GPIO.Monad
import System.GPIO.Types
       (Pin(..), PinActiveLevel(..), PinDirection(..),
        PinInterruptMode(..), PinValue(..), SomeGpioException)

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

testSetInterruptMode :: (MonadGpio h m) => m (Maybe PinInterruptMode, Maybe PinInterruptMode, Maybe PinInterruptMode, Maybe PinInterruptMode)
testSetInterruptMode =
  do d <- openPin (Pin 1)
     setPinDirection d In
     setPinInterruptMode d Disabled
     t1 <- getPinInterruptMode d
     setPinInterruptMode d RisingEdge
     t2 <- getPinInterruptMode d
     setPinInterruptMode d FallingEdge
     t3 <- getPinInterruptMode d
     setPinInterruptMode d Level
     t4 <- getPinInterruptMode d
     closePin d
     return (t1, t2, t3, t4)

testSetInterruptModeIdempotent :: (MonadGpio h m) => m (Maybe PinInterruptMode, Maybe PinInterruptMode)
testSetInterruptModeIdempotent =
  do d <- openPin (Pin 1)
     setPinDirection d In
     setPinInterruptMode d FallingEdge
     t1 <- getPinInterruptMode d
     setPinInterruptMode d FallingEdge
     t2 <- getPinInterruptMode d
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

testReadWritePin :: (MonadGpio h m) => h -> m (PinValue, PinValue, PinValue)
testReadWritePin d =
  do val1 <- readPin d
     case val1 of
       Low ->
         do writePin d High
            val2 <- readPin d
            writePin d Low
            val3 <- readPin d
            return (val1,val2,val3)
       High ->
         do writePin d Low
            val2 <- readPin d
            writePin d High
            val3 <- readPin d
            return (val1,val2,val3)

testReadWritePinIdempotent :: (MonadGpio h m) => m (PinValue, PinValue)
testReadWritePinIdempotent =
  do d <- openPin (Pin 1)
     setPinDirection d Out
     val1 <- readPin d
     case val1 of
       Low ->
         do writePin d Low
            val2 <- readPin d
            closePin d
            return (val1,val2)
       High ->
         do writePin d High
            val2 <- readPin d
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
     val1 <- readPin d1
     writePin' d2 Low
     val2 <- readPin d2
     dir1 <- getPinDirection d1
     dir2 <- getPinDirection d2
     closePin d1
     closePin d2
     return (val1, val2, dir1, dir2)

testWritePinIdempotent' :: (MonadGpio h m) => m (PinValue, PinValue, PinValue, PinValue)
testWritePinIdempotent' =
  do d <- openPin (Pin 1)
     writePin' d High
     val1 <- readPin d
     writePin' d High
     val2 <- readPin d
     writePin' d Low
     val3 <- readPin d
     writePin' d Low
     val4 <- readPin d
     closePin d
     return (val1, val2, val3, val4)

testTogglePinValue :: (MonadGpio h m) => m (PinValue, PinValue, PinValue, PinValue, PinValue)
testTogglePinValue =
  do d <- openPin (Pin 1)
     setPinDirection d Out
     val1 <- readPin d
     val2 <- togglePinValue d
     val3 <- readPin d
     val4 <- togglePinValue d
     val5 <- readPin d
     closePin d
     return (val1, val2, val3, val4, val5)

testTogglePinActiveLevel :: (MonadGpio h m) => h -> m (PinActiveLevel, PinActiveLevel, PinActiveLevel, PinActiveLevel, PinActiveLevel)
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
     val <- readPin h
     return val

testNestedWithPin :: (MonadGpio h m, MonadMask m) => Pin -> Pin -> m (PinValue, PinValue)
testNestedWithPin p1 p2 =
  withPin p1 $ \h1 ->
    withPin p2 $ \h2 ->
      do setPinDirection h1 Out
         setPinDirection h2 Out
         writePin h1 High
         writePin h2 Low
         val1 <- readPin h1
         val2 <- readPin h2
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
         val1 <- readPin h1
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
                   setPinInterruptMode h RisingEdge
                   setPinDirection h Out
                   setPinDirection h In
                   setPinInterruptMode h FallingEdge
                   setPinDirection h Out
                   setPinDirection h In
                   setPinInterruptMode h Level
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
                   setPinInterruptMode h RisingEdge
                   void $ togglePinDirection h
                   setPinDirection h In
                   setPinInterruptMode h FallingEdge
                   void $ togglePinDirection h
                   setPinDirection h In
                   setPinInterruptMode h Level
                   void $ togglePinDirection h
                   return True)
              initialMockWorld
              [chip0]
            `shouldBe` Right True
          it "returns Nothing when the pin direction is not settable" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_userVisibleDirection = False}]
            in evalSysfsGpioMock' (withPin (Pin 1) togglePinDirection) initialMockWorld [testChip] `shouldBe` Right Nothing

     describe "getPinInterruptMode" $
       do it "gets the pin's interrupt mode" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_edge = Just Falling}]
            in
              do evalSysfsGpioMock' (withPin (Pin 1) getPinInterruptMode) initialMockWorld [chip0] `shouldBe` Right (Just Disabled)
                 evalSysfsGpioMock' (withPin (Pin 1) getPinInterruptMode) initialMockWorld [testChip] `shouldBe` Right (Just FallingEdge)

          it "returns Nothing when the pin's interrupt mode is not settable" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_edge = Nothing}]
            in evalSysfsGpioMock' (withPin (Pin 1) getPinInterruptMode) initialMockWorld [testChip] `shouldBe` Right Nothing

     describe "setPinInterruptMode" $
       do it "sets the pin's interrupt mode" $
            evalSysfsGpioMock' testSetInterruptMode initialMockWorld [chip0] `shouldBe` Right (Just Disabled, Just RisingEdge, Just FallingEdge, Just Level)
          it "is idempotent" $
            evalSysfsGpioMock' testSetInterruptModeIdempotent initialMockWorld [chip0] `shouldBe` Right (Just FallingEdge, Just FallingEdge)
          it "fails when the pin's interrupt mode is not settable" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_edge = Nothing}, defaultMockPinState]
            in evalSysfsGpioMock' testSetInterruptMode initialMockWorld [testChip] `shouldBe` Left (Just $ NoEdgeAttribute (Pin 1))
          it "fails when the pin is configured for output" $
            evalSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinInterruptMode h Level) initialMockWorld [chip0] `shouldBe` Left (Just $ InvalidOperation (Pin 1))

     describe "getPinActiveLevel" $
       do context "when active level is high" $
            do it "returns the pin's active level" $
                   evalSysfsGpioMock' (withPin (Pin 1) getPinActiveLevel) initialMockWorld [chip0] `shouldBe` Right ActiveHigh
          context "when active level is low" $
            do it "returns the pin's active level" $
                 let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_activeLow = True}]
                 in evalSysfsGpioMock' (withPin (Pin 1) getPinActiveLevel) initialMockWorld [testChip] `shouldBe` Right ActiveLow

     describe "setPinActiveLevel" $
       do context "when active level is high" $
            do it "sets the pin's active level to low" $
                 let (Right (result, world)) = runSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h ActiveLow >> getPinActiveLevel h)) initialMockWorld [chip0]
                 in do result `shouldBe` ActiveLow
                       Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = True})
               it "sets the pin's active level to high" $
                 let (Right (result, world)) = runSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h ActiveHigh >> getPinActiveLevel h)) initialMockWorld [chip0]
                 in do result `shouldBe` ActiveHigh
                       Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = False})
          context "when active level is low" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_activeLow = True}]
            in do it "sets the pin's active level to low" $
                     let (Right (result, world)) = runSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h ActiveLow >> getPinActiveLevel h)) initialMockWorld [testChip]
                    in do result `shouldBe` ActiveLow
                          Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = True})
                  it "sets the pin's active level to high" $
                     let (Right (result, world)) = runSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h ActiveHigh >> getPinActiveLevel h)) initialMockWorld [chip0]
                    in do result `shouldBe` ActiveHigh
                          Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = False})

     describe "togglePinActiveLevel" $
       do it "toggles the pin's active level when the pin is configured for output" $
            evalSysfsGpioMock' (withPin (Pin 1) testTogglePinActiveLevel) initialMockWorld [chip0] `shouldBe` Right (ActiveHigh, ActiveLow, ActiveLow, ActiveHigh, ActiveHigh)
          it "and when the pin is configured for input" $
            evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinDirection h In >> testTogglePinActiveLevel h)) initialMockWorld [chip0] `shouldBe` Right (ActiveHigh, ActiveLow, ActiveLow, ActiveHigh, ActiveHigh)

     describe "pollPin" $
       do it "waits for the specified trigger and returns the pin's value" $
            pendingWith "need to implement this"

          it "blocks when the interrupt mode is Disabled, until it is changed" $
            pendingWith "need to implement this"

     describe "pollPinTimeout" $
       do it "waits for the specified trigger and returns the pin's value" $
            pendingWith "need to implement this"
          it "blocks when the interrupt mode is Disabled, until it is changed" $
            pendingWith "need to implement this"
          it "times out" $
            pendingWith "need to implement this"

     describe "readPin" $
       do context "returns the pin's logical level" $
            do it "when the active level is high" $
                 evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h ActiveHigh >> readPin h)) initialMockWorld [chip0] `shouldBe` Right Low
               it "when the active level is low" $
                 evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h ActiveLow >> readPin h)) initialMockWorld [chip0] `shouldBe` Right High

     describe "writePin" $
       do context "sets the pin's logical value" $
            do it "when the active level is high" $
                 let Right (result, world) = runSysfsGpioMock' (withPin (Pin 1) testReadWritePin) initialMockWorld [chip0]
                 in do result `shouldBe` (Low, High, Low)
                       Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState
               it "when the active level is low" $
                  let Right (result, world) = runSysfsGpioMock' (withPin (Pin 1) (\h -> setPinActiveLevel h ActiveLow >> testReadWritePin h)) initialMockWorld [chip0]
                 in do result `shouldBe` (High, Low, High)
                       Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just (defaultMockPinState {_activeLow = True, _value = Low})

          it "is idempotent" $
            evalSysfsGpioMock' testReadWritePinIdempotent initialMockWorld [chip0] `shouldBe` Right (Low, Low)

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
                   setPinInterruptMode h RisingEdge
                   writePin' h High
                   val1 <- readPin h
                   setPinDirection h In
                   setPinInterruptMode h RisingEdge
                   writePin' h High
                   val2 <- readPin h
                   setPinDirection h In
                   setPinInterruptMode h RisingEdge
                   writePin' h High
                   val3 <- readPin h
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

          it "in readPin" $
            evalSysfsGpioMock' (invalidHandle readPin) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in pollPin" $
            evalSysfsGpioMock' (invalidHandle pollPin) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in pollPinTimeout" $
            evalSysfsGpioMock' (invalidHandle (\h -> pollPinTimeout h 10000)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in writePin" $
             evalSysfsGpioMock' (invalidHandle (\d -> writePin d High)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in writePin'" $
             evalSysfsGpioMock' (invalidHandle (\d -> writePin' d High)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in togglePinValue" $
            evalSysfsGpioMock' (invalidHandle togglePinValue) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in getPinInterruptMode" $
            evalSysfsGpioMock' (invalidHandle getPinInterruptMode) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in setPinInterruptMode" $
             evalSysfsGpioMock' (invalidHandle (\d -> setPinInterruptMode d Level)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in getPinActiveLevel" $
            evalSysfsGpioMock' (invalidHandle getPinActiveLevel) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in setPinActiveLevel" $
            evalSysfsGpioMock' (invalidHandle (\d -> setPinActiveLevel d ActiveHigh)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

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

     describe "InputPin" $
       do context "withInputPin" $
            do it "opens and configures a pin for input, then closes closes it" $
                     let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In}, defaultMockPinState {_direction = Out}]
                         (Right world) =
                           execSysfsGpioMock'
                             (withInputPin (Pin 1) Nothing $ \_ ->
                                withInputPin (Pin 2) Nothing $ \_ ->
                                  return ()
                             )
                           initialMockWorld
                           [testChip]
                   in do evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio1") world [] `shouldBe` Right False
                         evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio2") world [] `shouldBe` Right False
                         Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In}
                         Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In}
               it "sets (or doesn't) the pin's active level" $
                     let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_activeLow = True}, defaultMockPinState {_activeLow = True}]
                         (Right world) =
                          execSysfsGpioMock'
                            (withInputPin (Pin 1) Nothing $ \_ ->
                               withInputPin (Pin 2) (Just ActiveHigh) $ \_ ->
                                 return ()
                            )
                            initialMockWorld
                            [testChip]
                     in do Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = True}
                           Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = False}
               it "fails if the pin's direction is fixed" $
                 let testChip = MockGpioChip "testChip" 1 [defaultMockPinState, defaultMockPinState {_direction = Out, _userVisibleDirection = False}]
                 in evalSysfsGpioMock'
                      (withInputPin (Pin 1) Nothing $ \_ ->
                         withInputPin (Pin 2) (Just ActiveHigh) $ \_ ->
                           return ()
                      )
                      initialMockWorld
                      [testChip]
                    `shouldBe`
                    Left (Just $ NoDirectionAttribute (Pin 2))
          context "readInputPin" $
            do it "respects the pin's active level" $
                  let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_value = High}, defaultMockPinState {_value = Low}]
                  in evalSysfsGpioMock'
                       (withInputPin (Pin 1) Nothing $ \h1 ->
                         withInputPin (Pin 2) (Just ActiveLow) $ \h2 ->
                           do v1 <- readInputPin h1
                              v2 <- readInputPin h2
                              return (v1,v2)
                        )
                       initialMockWorld
                       [testChip]
                     `shouldBe`
                     Right (High,High)
          context "get/setInputPinActiveLevel" $
             do it "gets/sets the pin's active level" $
                  let (Right (result, world)) =
                        runSysfsGpioMock'
                         (withInputPin (Pin 1) (Just ActiveHigh) $ \h1 ->
                            withInputPin (Pin 2) (Just ActiveLow) $ \h2 ->
                              do setInputPinActiveLevel h1 ActiveLow
                                 setInputPinActiveLevel h2 ActiveHigh
                                 l1 <- getInputPinActiveLevel h1
                                 l2 <- getInputPinActiveLevel h2
                                 return (l1,l2)
                         )
                         initialMockWorld
                         [chip0]
                  in do result `shouldBe` (ActiveLow,ActiveHigh)
                        Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = True}
                        Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = False}
          context "toggleInputPinActiveLevel" $
              do it "toggles the pin's active level and returns the new value" $
                   let (Right (result, world)) =
                         runSysfsGpioMock'
                          (withInputPin (Pin 1) (Just ActiveHigh) $ \h1 ->
                            withInputPin (Pin 2) (Just ActiveLow) $ \h2 ->
                              do l1 <- toggleInputPinActiveLevel h1
                                 l2 <- toggleInputPinActiveLevel h2
                                 return (l1,l2)
                          )
                          initialMockWorld
                          [chip0]
                   in do result `shouldBe` (ActiveLow,ActiveHigh)
                         Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = True}
                         Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = False}
     describe "InterruptPin" $
        do context "withInterruptPin" $
             do it "opens and configures a pin for interrupts, sets its interrupt mode, then closes closes it" $
                      let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In, _edge = Just None}, defaultMockPinState {_direction = Out, _edge = Just Rising}]
                          (Right world) =
                            execSysfsGpioMock'
                              (withInterruptPin (Pin 1) FallingEdge Nothing $ \_ ->
                                 withInterruptPin (Pin 2) Disabled Nothing $ \_ ->
                                   return ()
                              )
                            initialMockWorld
                            [testChip]
                    in do evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio1") world [] `shouldBe` Right False
                          evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio2") world [] `shouldBe` Right False
                          Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _edge = Just Falling}
                          Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _edge = Just None}
                it "sets (or doesn't) the pin's active level" $
                      let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_activeLow = True}, defaultMockPinState {_activeLow = True}]
                          (Right world) =
                           execSysfsGpioMock'
                             (withInterruptPin (Pin 1) Disabled Nothing $ \_ ->
                                withInterruptPin (Pin 2) Disabled (Just ActiveHigh) $ \_ ->
                                  return ()
                             )
                             initialMockWorld
                             [testChip]
                      in do Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = True}
                            Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = False}
                it "fails if the pin's direction is fixed" $
                  let testChip = MockGpioChip "testChip" 1 [defaultMockPinState, defaultMockPinState {_direction = Out, _userVisibleDirection = False}]
                  in evalSysfsGpioMock'
                       (withInterruptPin (Pin 1) Disabled Nothing $ \_ ->
                          withInterruptPin (Pin 2) Disabled (Just ActiveHigh) $ \_ ->
                            return ()
                       )
                       initialMockWorld
                       [testChip]
                     `shouldBe`
                     Left (Just $ NoDirectionAttribute (Pin 2))
                it "fails if the pin doesn't support interrupts" $
                   let testChip = MockGpioChip "testChip" 1 [defaultMockPinState, defaultMockPinState {_edge = Nothing}]
                   in evalSysfsGpioMock'
                        (withInterruptPin (Pin 1) Disabled Nothing $ \_ ->
                           withInterruptPin (Pin 2) Disabled (Just ActiveHigh) $ \_ ->
                             return ()
                        )
                        initialMockWorld
                        [testChip]
                      `shouldBe`
                      Left (Just $ NoEdgeAttribute (Pin 2))
           context "readInterruptPin" $
             do it "respects the pin's active level" $
                   let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_value = High}, defaultMockPinState {_value = Low}]
                   in evalSysfsGpioMock'
                        (withInterruptPin (Pin 1) Disabled Nothing $ \h1 ->
                          withInterruptPin (Pin 2) Disabled (Just ActiveLow) $ \h2 ->
                            do v1 <- readInterruptPin h1
                               v2 <- readInterruptPin h2
                               return (v1,v2)
                         )
                        initialMockWorld
                        [testChip]
                      `shouldBe`
                      Right (High,High)
           context "get/setInterruptPinActiveLevel" $
              do it "gets/sets the pin's active level" $
                   let (Right (result, world)) =
                         runSysfsGpioMock'
                          (withInterruptPin (Pin 1) Disabled (Just ActiveHigh) $ \h1 ->
                             withInterruptPin (Pin 2) Disabled (Just ActiveLow) $ \h2 ->
                               do setInterruptPinActiveLevel h1 ActiveLow
                                  setInterruptPinActiveLevel h2 ActiveHigh
                                  l1 <- getInterruptPinActiveLevel h1
                                  l2 <- getInterruptPinActiveLevel h2
                                  return (l1,l2)
                          )
                          initialMockWorld
                          [chip0]
                   in do result `shouldBe` (ActiveLow,ActiveHigh)
                         Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = True}
                         Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = False}
           context "toggleInterruptPinActiveLevel" $
               do it "toggles the pin's active level and returns the new value" $
                    let (Right (result, world)) =
                          runSysfsGpioMock'
                           (withInterruptPin (Pin 1) Disabled (Just ActiveHigh) $ \h1 ->
                             withInterruptPin (Pin 2) Disabled (Just ActiveLow) $ \h2 ->
                               do l1 <- toggleInterruptPinActiveLevel h1
                                  l2 <- toggleInterruptPinActiveLevel h2
                                  return (l1,l2)
                           )
                           initialMockWorld
                           [chip0]
                    in do result `shouldBe` (ActiveLow,ActiveHigh)
                          Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = True}
                          Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = False}
           context "get/setInterruptPinInterruptMode" $
               do it "gets/sets the pin's interrupt mode" $
                    let (Right (result, world)) =
                          runSysfsGpioMock'
                           (withInterruptPin (Pin 1) Disabled Nothing $ \h1 ->
                              withInterruptPin (Pin 2) RisingEdge Nothing $ \h2 ->
                                do setInterruptPinInterruptMode h1 RisingEdge
                                   setInterruptPinInterruptMode h2 Disabled
                                   m1 <- getInterruptPinInterruptMode h1
                                   m2 <- getInterruptPinInterruptMode h2
                                   return (m1,m2)
                           )
                           initialMockWorld
                           [chip0]
                    in do result `shouldBe` (RisingEdge,Disabled)
                          Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _edge = Just Rising}
                          Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _edge = Just None}
           context "pollInterruptPin" $
             do it "waits for interrupts" $
                  pendingWith "need to implement this"
                it "respects the pin's active level" $
                  pendingWith "need to implement this"

           context "pollInterruptPin" $
             do it "waits for interrupts" $
                  pendingWith "need to implement this"
                it "times out" $
                  pendingWith "need to implement this"
                it "respects the pin's active level" $
                  pendingWith "need to implement this"
     describe "OutputPin" $
        do context "withOutputPin" $
             do it "opens and configures a pin for output, sets its value, then closes closes it" $
                      let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In}, defaultMockPinState {_direction = Out, _value = High}]
                          (Right world) =
                            execSysfsGpioMock'
                              (withOutputPin (Pin 1) Nothing High $ \_ ->
                                 withOutputPin (Pin 2) Nothing Low $ \_ ->
                                   return ()
                              )
                            initialMockWorld
                            [testChip]
                    in do evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio1") world [] `shouldBe` Right False
                          evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio2") world [] `shouldBe` Right False
                          Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _value = High}
                          Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _value = Low}
                it "sets (or doesn't) the pin's active level (and the output value is relative to it)" $
                      let (Right world) =
                           execSysfsGpioMock'
                             (withOutputPin (Pin 1) Nothing Low $ \_ ->
                                withOutputPin (Pin 2) Nothing High $ \_ ->
                                  withOutputPin (Pin 3) (Just ActiveLow) Low $ \_ ->
                                    withOutputPin (Pin 4) (Just ActiveLow) High $ \_ ->
                                      withOutputPin (Pin 5) (Just ActiveHigh) Low $ \_ ->
                                        withOutputPin (Pin 6) (Just ActiveHigh) High $ \_ ->
                                          return ()
                             )
                             initialMockWorld
                             [chip0]
                      in do Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState
                            Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _value = High}
                            Map.lookup (Pin 3) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = True, _value = High}
                            Map.lookup (Pin 4) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = True, _value = Low}
                            Map.lookup (Pin 5) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _value = Low}
                            Map.lookup (Pin 6) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _value = High}
                it "fails if the pin's direction is fixed" $
                  let testChip = MockGpioChip "testChip" 1 [defaultMockPinState, defaultMockPinState {_direction = Out, _userVisibleDirection = False}]
                  in evalSysfsGpioMock'
                       (withOutputPin (Pin 1) Nothing High $ \_ ->
                          withOutputPin (Pin 2) Nothing Low $ \_ ->
                            return ()
                       )
                       initialMockWorld
                       [testChip]
                     `shouldBe`
                     Left (Just $ NoDirectionAttribute (Pin 2))
           context "readOutputPin" $
             do it "respects the pin's active level" $
                  evalSysfsGpioMock'
                    (withOutputPin (Pin 1) Nothing Low $ \h1 ->
                      withOutputPin (Pin 2) (Just ActiveLow) High $ \h2 ->
                        do v1 <- readOutputPin h1
                           v2 <- readOutputPin h2
                           return (v1,v2)
                     )
                     initialMockWorld
                     [chip0]
                     `shouldBe`
                     Right (Low,High)
           context "writeOutputPin" $
             do it "writes the output value and respects the pin's active level" $
                  let (Right world) =
                        execSysfsGpioMock'
                          (withOutputPin (Pin 1) (Just ActiveLow) High $ \h1 ->
                             withOutputPin (Pin 2) (Just ActiveLow) Low $ \h2 ->
                               withOutputPin (Pin 3) (Just ActiveHigh) High $ \h3 ->
                                 withOutputPin (Pin 4) (Just ActiveHigh) Low $ \h4 ->
                                   do writeOutputPin h1 Low
                                      writeOutputPin h2 High
                                      writeOutputPin h3 Low
                                      writeOutputPin h4 High
                          )
                          initialMockWorld
                          [chip0]
                  in do Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = True, _value = High}
                        Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = True, _value = Low}
                        Map.lookup (Pin 3) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _value = Low}
                        Map.lookup (Pin 4) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _value = High}
           context "toggleOutputPin" $
              do it "toggles the output value, returns the new value, and respects the pin's active level" $
                   let (Right (result, world)) =
                         runSysfsGpioMock'
                           (withOutputPin (Pin 1) (Just ActiveLow) High $ \h1 ->
                              withOutputPin (Pin 2) (Just ActiveLow) Low $ \h2 ->
                                withOutputPin (Pin 3) (Just ActiveHigh) High $ \h3 ->
                                  withOutputPin (Pin 4) (Just ActiveHigh) Low $ \h4 ->
                                    do v1 <- toggleOutputPin h1
                                       v2 <- toggleOutputPin h2
                                       v3 <- toggleOutputPin h3
                                       v4 <- toggleOutputPin h4
                                       return (v1,v2,v3,v4)
                           )
                           initialMockWorld
                           [chip0]
                   in do result `shouldBe` (Low,High,Low,High)
                         Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = True, _value = High}
                         Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = True, _value = Low}
                         Map.lookup (Pin 3) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _value = Low}
                         Map.lookup (Pin 4) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _value = High}
           context "get/setOutputPinActiveLevel" $
              do it "gets/sets the pin's active level" $
                   let (Right (result, world)) =
                         runSysfsGpioMock'
                          (withOutputPin (Pin 1) (Just ActiveHigh) Low $ \h1 ->
                             withOutputPin (Pin 2) (Just ActiveLow) Low $ \h2 ->
                               do setOutputPinActiveLevel h1 ActiveLow
                                  setOutputPinActiveLevel h2 ActiveHigh
                                  l1 <- getOutputPinActiveLevel h1
                                  l2 <- getOutputPinActiveLevel h2
                                  return (l1,l2)
                          )
                          initialMockWorld
                          [chip0]
                   in do result `shouldBe` (ActiveLow,ActiveHigh)
                         Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = True}
                         Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = False, _value = High}
           context "toggleOutputPinActiveLevel" $
               do it "toggles the pin's active level and returns the new value" $
                    let (Right (result, world)) =
                          runSysfsGpioMock'
                           (withOutputPin (Pin 1) (Just ActiveHigh) Low $ \h1 ->
                             withOutputPin (Pin 2) (Just ActiveLow) Low $ \h2 ->
                               do l1 <- toggleOutputPinActiveLevel h1
                                  l2 <- toggleOutputPinActiveLevel h2
                                  return (l1,l2)
                           )
                           initialMockWorld
                           [chip0]
                    in do result `shouldBe` (ActiveLow,ActiveHigh)
                          Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = True}
                          Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = False, _value = High}
