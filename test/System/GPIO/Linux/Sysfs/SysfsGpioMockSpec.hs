{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.SysfsGpioMockSpec (spec) where

import Control.Exception (fromException)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, handle)
import qualified Data.Map.Strict as Map (lookup)
import qualified Data.Set as Set (empty, fromList)

import System.GPIO.Linux.Sysfs.Mock
import System.GPIO.Linux.Sysfs.Types (SysfsEdge(..), SysfsException(..))
import System.GPIO.Monad
import System.GPIO.Types
       (Pin(..), PinInputMode(..), PinOutputMode(..), PinCapabilities(..),
        PinActiveLevel(..), PinDirection(..), PinInterruptMode(..),
        PinValue(..), SomeGpioException)

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

testSetInterruptMode :: (MonadGpio h m) => m (Maybe PinInterruptMode, Maybe PinInterruptMode, Maybe PinInterruptMode, Maybe PinInterruptMode)
testSetInterruptMode =
  do d <- openPin (Pin 1)
     setPinInputMode d InputDefault
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
     setPinInputMode d InputDefault
     setPinInterruptMode d FallingEdge
     t1 <- getPinInterruptMode d
     setPinInterruptMode d FallingEdge
     t2 <- getPinInterruptMode d
     closePin d
     return (t1, t2)

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
     setPinOutputMode d OutputDefault Low
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
     setPinInputMode d InputDefault
     writePin d High
     closePin d

testTogglePinValue :: (MonadGpio h m) => m (PinValue, PinValue, PinValue, PinValue, PinValue)
testTogglePinValue =
  do d <- openPin (Pin 1)
     setPinOutputMode d OutputDefault Low
     val1 <- readPin d
     val2 <- togglePin d
     val3 <- readPin d
     val4 <- togglePin d
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
  do setPinOutputMode h OutputDefault Low
     writePin h High
     val <- readPin h
     return val

testNestedWithPin :: (MonadGpio h m, MonadMask m) => Pin -> Pin -> m (PinValue, PinValue)
testNestedWithPin p1 p2 =
  withPin p1 $ \h1 ->
    withPin p2 $ \h2 ->
      do setPinOutputMode h1 OutputDefault Low
         setPinOutputMode h2 OutputDefault Low
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
      do setPinOutputMode h1 OutputDefault Low
         setPinInputMode h2 InputDefault
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

     describe "pinCapabilities" $
       do it "returns a pin's capabilities" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState
                                                     ,defaultMockPinState {_userVisibleDirection = False}
                                                     ,defaultMockPinState {_edge = Nothing}]
                (Right result) =
                  evalSysfsGpioMock'
                    (do c1 <- pinCapabilities (Pin 1)
                        c2 <- pinCapabilities (Pin 2)
                        c3 <- pinCapabilities (Pin 3)
                        return (c1,c2,c3)
                    )
                    initialMockWorld
                    [testChip]
            in do result
                    `shouldBe`
                    (PinCapabilities (Set.fromList [InputDefault]) (Set.fromList [OutputDefault]) True,
                     PinCapabilities Set.empty Set.empty False,
                     PinCapabilities (Set.fromList [InputDefault]) (Set.fromList [OutputDefault]) False)
          it "doesn't leave any pin state around" $
            let (Right world) =
                  execSysfsGpioMock (void $ pinCapabilities (Pin 1)) initialMockWorld [chip0]
            in evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio1") world [] `shouldBe` Right False
          it "fails if the pin doesn't exist" $
            let (Left failure) =
                  evalSysfsGpioMock' (pinCapabilities (Pin 99)) initialMockWorld [chip0]
            in failure `shouldBe` (Just $ InvalidPin (Pin 99))

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
              do evalSysfsGpioMock' (withPin (Pin 1) getPinDirection) initialMockWorld [chip0] `shouldBe` Right Out
                 evalSysfsGpioMock' (withPin (Pin 1) getPinDirection) initialMockWorld [testChip] `shouldBe` Right In

          it "fails when the pin direction is not settable" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_userVisibleDirection = False}]
            in evalSysfsGpioMock' (withPin (Pin 1) getPinDirection) initialMockWorld [testChip] `shouldBe` Left (Just $ NoDirectionAttribute (Pin 1))

     describe "getPinInputMode" $
       do it "gets the pin's input mode" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In}]
            in
              evalSysfsGpioMock' (withPin (Pin 1) getPinInputMode) initialMockWorld [testChip] `shouldBe` Right InputDefault
          it "fails when the pin's direction is Out" $
            evalSysfsGpioMock' (withPin (Pin 1) getPinInputMode) initialMockWorld [chip0] `shouldBe` Left (Just $ InvalidOperation (Pin 1))

     describe "setPinInputMode" $
       do it "sets the pin's input mode and direction" $
             let testChip1 = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = Out, _edge = Just Falling, _activeLow = True}]
                 testChip2 = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In, _edge = Just Rising}]
                 (Right (result1,world1)) = runSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinInputMode h InputDefault >> getPinInputMode h) initialMockWorld [testChip1]
                 (Right (result2,world2)) = runSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinInputMode h InputDefault >> getPinInputMode h) initialMockWorld [testChip2]
             in do result1 `shouldBe` InputDefault
                   result2 `shouldBe` InputDefault
                   Map.lookup (Pin 1) (mockWorldPins world1) `shouldBe` Just (defaultMockPinState {_direction = In, _edge = Just Falling, _activeLow = True})
                   Map.lookup (Pin 1) (mockWorldPins world2) `shouldBe` Just (defaultMockPinState {_direction = In, _edge = Just Rising})

          it "fails when the input mode is unsupported" $
             evalSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinInputMode h InputFloating) initialMockWorld [chip0] `shouldBe` Left (Just $ (UnsupportedInputMode InputFloating (Pin 1)))
          it "fails when the pin's direction is not settable" $
             let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_userVisibleDirection = False}]
             in evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinInputMode h InputDefault)) initialMockWorld [testChip] `shouldBe` Left (Just $ NoDirectionAttribute (Pin 1))

     describe "getPinOutputMode" $
       do it "gets the pin's output mode" $
            evalSysfsGpioMock' (withPin (Pin 1) getPinOutputMode) initialMockWorld [chip0] `shouldBe` Right OutputDefault
          it "fails when the pin's direction is Out" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In}]
            in evalSysfsGpioMock' (withPin (Pin 1) getPinOutputMode) initialMockWorld [testChip] `shouldBe` Left (Just $ InvalidOperation (Pin 1))

     describe "setPinOutputMode" $
       do it "sets the pin's output mode, value, and direction" $
            let testChip1 = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In, _value = Low}]
                (Right (result1,world1)) = runSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinOutputMode h OutputDefault High >> getPinOutputMode h) initialMockWorld [testChip1]
                testChip2 = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In, _value = Low}]
                (Right (result2,world2)) = runSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinOutputMode h OutputDefault Low >> getPinOutputMode h) initialMockWorld [testChip2]
                testChip3 = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = Out, _value = High}]
                (Right (result3,world3)) = runSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinOutputMode h OutputDefault Low >> getPinOutputMode h) initialMockWorld [testChip3]
                (Right (result4,world4)) = runSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinOutputMode h OutputDefault High >> getPinOutputMode h) initialMockWorld [chip0]
            in do result1 `shouldBe` OutputDefault
                  result2 `shouldBe` OutputDefault
                  result3 `shouldBe` OutputDefault
                  result4 `shouldBe` OutputDefault
                  Map.lookup (Pin 1) (mockWorldPins world1) `shouldBe` Just (defaultMockPinState {_direction = Out, _value = High})
                  Map.lookup (Pin 1) (mockWorldPins world2) `shouldBe` Just (defaultMockPinState {_direction = Out, _value = Low})
                  Map.lookup (Pin 1) (mockWorldPins world3) `shouldBe` Just (defaultMockPinState {_direction = Out, _value = Low})
                  Map.lookup (Pin 1) (mockWorldPins world4) `shouldBe` Just (defaultMockPinState {_direction = Out, _value = High})
          it "respects the pin's active level" $
            let testChip1 = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In, _value = Low, _activeLow = True}]
                (Right (result1,world1)) = runSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinOutputMode h OutputDefault High >> getPinOutputMode h) initialMockWorld [testChip1]
                testChip2 = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = In, _value = Low, _activeLow = True}]
                (Right (result2,world2)) = runSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinOutputMode h OutputDefault Low >> getPinOutputMode h) initialMockWorld [testChip2]
                testChip3 = MockGpioChip "testChip" 1 [defaultMockPinState {_direction = Out, _value = High, _activeLow = True}]
                (Right (result3,world3)) = runSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinOutputMode h OutputDefault Low >> getPinOutputMode h) initialMockWorld [testChip3]
                testChip4 = MockGpioChip "testChip" 1 [defaultMockPinState {_activeLow = True}]
                (Right (result4,world4)) = runSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinOutputMode h OutputDefault High >> getPinOutputMode h) initialMockWorld [testChip4]
             in do result1 `shouldBe` OutputDefault
                   result2 `shouldBe` OutputDefault
                   result3 `shouldBe` OutputDefault
                   result4 `shouldBe` OutputDefault
                   Map.lookup (Pin 1) (mockWorldPins world1) `shouldBe` Just (defaultMockPinState {_direction = Out, _activeLow = True, _value = Low})
                   Map.lookup (Pin 1) (mockWorldPins world2) `shouldBe` Just (defaultMockPinState {_direction = Out, _activeLow = True, _value = High})
                   Map.lookup (Pin 1) (mockWorldPins world3) `shouldBe` Just (defaultMockPinState {_direction = Out, _activeLow = True, _value = High})
                   Map.lookup (Pin 1) (mockWorldPins world4) `shouldBe` Just (defaultMockPinState {_direction = Out, _activeLow = True, _value = Low})
          it "fails when the output mode is unsupported" $
              evalSysfsGpioMock' (withPin (Pin 1) $ \h -> setPinOutputMode h OutputPushPull Low) initialMockWorld [chip0] `shouldBe` Left (Just $ (UnsupportedOutputMode OutputPushPull (Pin 1)))
          it "can change the pin direction to 'Out' when the pin is configured for edge- or level-triggered reads " $
            evalSysfsGpioMock'
              (withPin (Pin 1) $ \h ->
                do setPinInputMode h InputDefault
                   setPinInterruptMode h RisingEdge
                   setPinOutputMode h OutputDefault Low
                   setPinInputMode h InputDefault
                   setPinInterruptMode h FallingEdge
                   setPinOutputMode h OutputDefault Low
                   setPinInputMode h InputDefault
                   setPinInterruptMode h Level
                   setPinOutputMode h OutputDefault Low
                   return True)
              initialMockWorld
              [chip0]
            `shouldBe` Right True
          it "fails when the pin's direction is not settable" $
            let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_userVisibleDirection = False}]
            in evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinOutputMode h OutputDefault Low)) initialMockWorld [testChip] `shouldBe` Left (Just $ NoDirectionAttribute (Pin 1))

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
            evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinInputMode h InputDefault >> testTogglePinActiveLevel h)) initialMockWorld [chip0] `shouldBe` Right (ActiveHigh, ActiveLow, ActiveLow, ActiveHigh, ActiveHigh)

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

     describe "togglePin" $
       do it "toggles the pin's value" $
            evalSysfsGpioMock' testTogglePinValue initialMockWorld [chip0] `shouldBe` Right (Low, High, High, Low, Low)
          it "fails when the pin is not configured for output" $
            evalSysfsGpioMock' (withPin (Pin 1) (\h -> setPinInputMode h InputDefault >> togglePin h)) initialMockWorld [chip0] `shouldBe` Left (Just $ PermissionDenied (Pin 1))

     describe "operations on an invalid handle fail" $
       -- Note: When used on an invalid handle, GPIO commands which
       -- return a 'Maybe' result will fail differently than commands
       -- which do not. This is by design.
       do it "in getPinDirection" $
             evalSysfsGpioMock' (invalidHandle getPinDirection) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in getPinInputMode" $
            evalSysfsGpioMock' (invalidHandle getPinInputMode) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in setPinInputMode" $
            evalSysfsGpioMock' (invalidHandle (\d -> setPinInputMode d InputDefault)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in getPinOutputMode" $
             evalSysfsGpioMock' (invalidHandle getPinOutputMode) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in setPinOutputMode" $
             evalSysfsGpioMock' (invalidHandle (\h -> setPinOutputMode h OutputDefault Low)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in readPin" $
            evalSysfsGpioMock' (invalidHandle readPin) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in pollPin" $
            evalSysfsGpioMock' (invalidHandle pollPin) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in pollPinTimeout" $
            evalSysfsGpioMock' (invalidHandle (\h -> pollPinTimeout h 10000)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in writePin" $
             evalSysfsGpioMock' (invalidHandle (\d -> writePin d High)) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

          it "in togglePin" $
            evalSysfsGpioMock' (invalidHandle togglePin) initialMockWorld [chip0] `shouldBe` Left (Just $ NotExported (Pin 1))

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
                             (withInputPin (Pin 1) InputDefault Nothing $ \_ ->
                                withInputPin (Pin 2) InputDefault Nothing $ \_ ->
                                  return ()
                             )
                           initialMockWorld
                           [testChip]
                   in do evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio1") world [] `shouldBe` Right False
                         evalSysfsMock' (doesDirectoryExist "/sys/class/gpio/gpio2") world [] `shouldBe` Right False
                         Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In}
                         Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In}
               it "sets the pin's input mode" $
                 evalSysfsGpioMock'
                     (withInputPin (Pin 1) InputDefault Nothing $ \h ->
                        getInputPinInputMode h
                     )
                     initialMockWorld
                     [chip0]
                   `shouldBe` Right InputDefault
               it "sets (or doesn't) the pin's active level" $
                     let testChip = MockGpioChip "testChip" 1 [defaultMockPinState {_activeLow = True}, defaultMockPinState {_activeLow = True}]
                         (Right world) =
                          execSysfsGpioMock'
                            (withInputPin (Pin 1) InputDefault Nothing $ \_ ->
                               withInputPin (Pin 2) InputDefault (Just ActiveHigh) $ \_ ->
                                 return ()
                            )
                            initialMockWorld
                            [testChip]
                     in do Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = True}
                           Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = False}
               it "fails if the pin's direction is fixed" $
                 let testChip = MockGpioChip "testChip" 1 [defaultMockPinState, defaultMockPinState {_direction = Out, _userVisibleDirection = False}]
                 in evalSysfsGpioMock'
                      (withInputPin (Pin 1) InputDefault Nothing $ \_ ->
                         withInputPin (Pin 2) InputDefault (Just ActiveHigh) $ \_ ->
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
                       (withInputPin (Pin 1) InputDefault Nothing $ \h1 ->
                         withInputPin (Pin 2) InputDefault (Just ActiveLow) $ \h2 ->
                           do v1 <- readInputPin h1
                              v2 <- readInputPin h2
                              return (v1,v2)
                        )
                       initialMockWorld
                       [testChip]
                     `shouldBe`
                     Right (High,High)
          context "get/setInputPinInputMode" $
            do it "gets/sets the pin's input mode" $
                 evalSysfsGpioMock'
                    (withInputPin (Pin 1) InputDefault Nothing $ \h ->
                       do setInputPinInputMode h InputDefault
                          getInputPinInputMode h
                    )
                    initialMockWorld
                    [chip0]
                  `shouldBe` Right InputDefault
               it "fails when the input mode is unsupported" $
                 evalSysfsGpioMock'
                   (withInputPin (Pin 1) InputDefault Nothing $ \h ->
                      setInputPinInputMode h InputPullDown
                   )
                   initialMockWorld
                   [chip0]
                 `shouldBe` Left (Just (UnsupportedInputMode InputPullDown (Pin 1)))
          context "get/setInputPinActiveLevel" $
             do it "gets/sets the pin's active level" $
                  let (Right (result, world)) =
                        runSysfsGpioMock'
                         (withInputPin (Pin 1) InputDefault (Just ActiveHigh) $ \h1 ->
                            withInputPin (Pin 2) InputDefault (Just ActiveLow) $ \h2 ->
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
                          (withInputPin (Pin 1) InputDefault (Just ActiveHigh) $ \h1 ->
                            withInputPin (Pin 2) InputDefault (Just ActiveLow) $ \h2 ->
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
                              (withInterruptPin (Pin 1) InputDefault FallingEdge Nothing $ \_ ->
                                 withInterruptPin (Pin 2) InputDefault Disabled Nothing $ \_ ->
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
                             (withInterruptPin (Pin 1) InputDefault Disabled Nothing $ \_ ->
                                withInterruptPin (Pin 2) InputDefault Disabled (Just ActiveHigh) $ \_ ->
                                  return ()
                             )
                             initialMockWorld
                             [testChip]
                      in do Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = True}
                            Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = In, _activeLow = False}
                it "fails if the pin's direction is fixed" $
                  let testChip = MockGpioChip "testChip" 1 [defaultMockPinState, defaultMockPinState {_direction = Out, _userVisibleDirection = False}]
                  in evalSysfsGpioMock'
                       (withInterruptPin (Pin 1) InputDefault Disabled Nothing $ \_ ->
                          withInterruptPin (Pin 2) InputDefault Disabled (Just ActiveHigh) $ \_ ->
                            return ()
                       )
                       initialMockWorld
                       [testChip]
                     `shouldBe`
                     Left (Just $ NoDirectionAttribute (Pin 2))
                it "fails if the pin doesn't support interrupts" $
                   let testChip = MockGpioChip "testChip" 1 [defaultMockPinState, defaultMockPinState {_edge = Nothing}]
                   in evalSysfsGpioMock'
                        (withInterruptPin (Pin 1) InputDefault Disabled Nothing $ \_ ->
                           withInterruptPin (Pin 2) InputDefault Disabled (Just ActiveHigh) $ \_ ->
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
                        (withInterruptPin (Pin 1) InputDefault Disabled Nothing $ \h1 ->
                          withInterruptPin (Pin 2) InputDefault Disabled (Just ActiveLow) $ \h2 ->
                            do v1 <- readInterruptPin h1
                               v2 <- readInterruptPin h2
                               return (v1,v2)
                         )
                        initialMockWorld
                        [testChip]
                      `shouldBe`
                      Right (High,High)
           context "get/setInterruptPinInputMode" $
             do it "gets/sets the pin's input mode" $
                  evalSysfsGpioMock'
                     (withInterruptPin (Pin 1) InputDefault Disabled Nothing $ \h ->
                        do setInterruptPinInputMode h InputDefault
                           getInterruptPinInputMode h
                     )
                     initialMockWorld
                     [chip0]
                   `shouldBe` Right InputDefault
                it "fails when the input mode is unsupported" $
                  evalSysfsGpioMock'
                    (withInterruptPin (Pin 1) InputDefault Disabled Nothing $ \h ->
                       setInterruptPinInputMode h InputPullDown
                    )
                    initialMockWorld
                    [chip0]
                  `shouldBe` Left (Just (UnsupportedInputMode InputPullDown (Pin 1)))
           context "get/setInterruptPinActiveLevel" $
              do it "gets/sets the pin's active level" $
                   let (Right (result, world)) =
                         runSysfsGpioMock'
                          (withInterruptPin (Pin 1) InputDefault Disabled (Just ActiveHigh) $ \h1 ->
                             withInterruptPin (Pin 2) InputDefault Disabled (Just ActiveLow) $ \h2 ->
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
                           (withInterruptPin (Pin 1) InputDefault Disabled (Just ActiveHigh) $ \h1 ->
                             withInterruptPin (Pin 2) InputDefault Disabled (Just ActiveLow) $ \h2 ->
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
                           (withInterruptPin (Pin 1) InputDefault Disabled Nothing $ \h1 ->
                              withInterruptPin (Pin 2) InputDefault RisingEdge Nothing $ \h2 ->
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
                              (withOutputPin (Pin 1) OutputDefault Nothing High $ \_ ->
                                 withOutputPin (Pin 2) OutputDefault Nothing Low $ \_ ->
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
                             (withOutputPin (Pin 1) OutputDefault Nothing Low $ \_ ->
                                withOutputPin (Pin 2) OutputDefault Nothing High $ \_ ->
                                  withOutputPin (Pin 3) OutputDefault (Just ActiveLow) Low $ \_ ->
                                    withOutputPin (Pin 4) OutputDefault (Just ActiveLow) High $ \_ ->
                                      withOutputPin (Pin 5) OutputDefault (Just ActiveHigh) Low $ \_ ->
                                        withOutputPin (Pin 6) OutputDefault (Just ActiveHigh) High $ \_ ->
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
                       (withOutputPin (Pin 1) OutputDefault Nothing High $ \_ ->
                          withOutputPin (Pin 2) OutputDefault Nothing Low $ \_ ->
                            return ()
                       )
                       initialMockWorld
                       [testChip]
                     `shouldBe`
                     Left (Just $ NoDirectionAttribute (Pin 2))
           context "readOutputPin" $
             do it "respects the pin's active level" $
                  evalSysfsGpioMock'
                    (withOutputPin (Pin 1) OutputDefault Nothing Low $ \h1 ->
                      withOutputPin (Pin 2) OutputDefault (Just ActiveLow) High $ \h2 ->
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
                          (withOutputPin (Pin 1) OutputDefault (Just ActiveLow) High $ \h1 ->
                             withOutputPin (Pin 2) OutputDefault (Just ActiveLow) Low $ \h2 ->
                               withOutputPin (Pin 3) OutputDefault (Just ActiveHigh) High $ \h3 ->
                                 withOutputPin (Pin 4) OutputDefault (Just ActiveHigh) Low $ \h4 ->
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
                           (withOutputPin (Pin 1) OutputDefault (Just ActiveLow) High $ \h1 ->
                              withOutputPin (Pin 2) OutputDefault (Just ActiveLow) Low $ \h2 ->
                                withOutputPin (Pin 3) OutputDefault (Just ActiveHigh) High $ \h3 ->
                                  withOutputPin (Pin 4) OutputDefault (Just ActiveHigh) Low $ \h4 ->
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
           context "get/setOutputPinOutputMode" $
             do it "gets/sets the pin's input mode" $
                  evalSysfsGpioMock'
                     (withOutputPin (Pin 1) OutputDefault Nothing Low $ \h ->
                        do setOutputPinOutputMode h OutputDefault Low
                           getOutputPinOutputMode h
                     )
                     initialMockWorld
                     [chip0]
                   `shouldBe` Right OutputDefault
                it "fails when the input mode is unsupported" $
                  evalSysfsGpioMock'
                    (withOutputPin (Pin 1) OutputDefault Nothing Low $ \h ->
                       setOutputPinOutputMode h OutputOpenSourcePullDown Low
                    )
                    initialMockWorld
                    [chip0]
                  `shouldBe` Left (Just (UnsupportedOutputMode OutputOpenSourcePullDown (Pin 1)))
           context "get/setOutputPinActiveLevel" $
              do it "gets/sets the pin's active level" $
                   let (Right (result, world)) =
                         runSysfsGpioMock'
                          (withOutputPin (Pin 1) OutputDefault (Just ActiveHigh) Low $ \h1 ->
                             withOutputPin (Pin 2) OutputDefault (Just ActiveLow) Low $ \h2 ->
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
                           (withOutputPin (Pin 1) OutputDefault (Just ActiveHigh) Low $ \h1 ->
                             withOutputPin (Pin 2) OutputDefault (Just ActiveLow) Low $ \h2 ->
                               do l1 <- toggleOutputPinActiveLevel h1
                                  l2 <- toggleOutputPinActiveLevel h2
                                  return (l1,l2)
                           )
                           initialMockWorld
                           [chip0]
                    in do result `shouldBe` (ActiveLow,ActiveHigh)
                          Map.lookup (Pin 1) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = True}
                          Map.lookup (Pin 2) (mockWorldPins world) `shouldBe` Just defaultMockPinState {_direction = Out, _activeLow = False, _value = High}
