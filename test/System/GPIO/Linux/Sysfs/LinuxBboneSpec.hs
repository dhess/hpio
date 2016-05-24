{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module System.GPIO.Linux.Sysfs.LinuxBboneSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory (doesDirectoryExist)
import System.GPIO.Linux.Sysfs (SysfsException(..), runSysfsGpioIO)
import System.GPIO.Monad (MonadGpio(..), withPin)
import System.GPIO.Types (Pin(..), PinDirection(..), PinInterruptMode(..), PinValue(..))
import Test.Hspec

isInvalidPinError :: SysfsException -> Bool
isInvalidPinError (InvalidPin _) = True
isInvalidPinError _ = False

isNotExportedError :: SysfsException -> Bool
isNotExportedError (NotExported _) = True
isNotExportedError _ = False

isPermissionDeniedError :: SysfsException -> Bool
isPermissionDeniedError (PermissionDenied _) = True
isPermissionDeniedError _ = False

isInvalidOperationError :: SysfsException -> Bool
isInvalidOperationError (InvalidOperation _) = True
isInvalidOperationError _ = False

-- Note: it's not practical to test all exceptional cases, but we do
-- our best.

-- Note: make sure the tests are always compiled, but only actually
-- run on the proper platform.
spec :: Spec
spec =
#ifdef RUN_LINUX_BBONE_TESTS
  runTests
#else
  describe "Linux BeagleBone tests disabled on this platform" $ return ()
#endif

-- NOTE: In order to run these tests properly, the following must be true:
--
-- * They must be run on BeagleBone Black hardware running Linux.
-- * You must have permission to export and unexport pins via the
--   sysfs GPIO filesystem.
-- * You must have permission to write pin attributes via the sysfs
--   GPIO filesystem.
-- * GPIO pins P9-15 (sysfs GPIO pin number 48) and P8-15 (sysfs GPIO
--   pin number 47) must be jumpered together.

testPin1 :: Pin
testPin1 = Pin 48

testPin2 :: Pin
testPin2 = Pin 47

invalidPin :: Pin
invalidPin = Pin 9000

-- Note: tests which modify pin state have a slight delay between
-- opening the pin and modifying any state. This is so that, if the
-- user running the tests is not root and is using a udev action to
-- change pin ownership (which is the only way at the time of writing
-- to do that), the script has time to run.

udevScriptWait :: MonadIO m => m ()
udevScriptWait = liftIO $ threadDelay 500000

runTests :: Spec
runTests =
  do
    describe "runSysfsGpioIO" $
      do context "pins" $
           it "returns the full list of pins on the system" $
             runSysfsGpioIO pins `shouldReturn` (map Pin [0..127])
         context "openPin/closePin" $
           do it "exports/unexports the pin" $
                runSysfsGpioIO
                  (do h <- openPin testPin1
                      exported <- liftIO $ doesDirectoryExist "/sys/class/gpio/gpio48"
                      closePin h
                      stillExported <- liftIO $ doesDirectoryExist "/sys/class/gpio/gpio48"
                      return (exported, stillExported))
                `shouldReturn` (True, False)
              it "openPin doesn't complain if the pin is already exported" $
                  runSysfsGpioIO
                    (withPin testPin1 $ \_ ->
                       do h <- openPin testPin1
                          void $ getPinDirection h
                          return True)
                    `shouldReturn` True
              it "openPin fails if the pin is invalid" $
                  runSysfsGpioIO
                    (do h <- openPin invalidPin
                        dir <- getPinDirection h
                        return dir)
                    `shouldThrow` isInvalidPinError
              it "closePin doesn't complain the pin is already unexported" $
                  runSysfsGpioIO
                    (withPin testPin1 $ \_ ->
                       do h <- openPin testPin1
                          closePin h
                          closePin h)
                    `shouldReturn` ()
         context "withPin" $
           do it "exports/unexports the pin" $
                do runSysfsGpioIO
                     (withPin testPin1 $ const $
                        liftIO $ doesDirectoryExist "/sys/class/gpio/gpio48")
                     `shouldReturn` True
                   doesDirectoryExist "/sys/class/gpio/gpio48" `shouldReturn` False
              it "unexports the pin upon exception" $
                do runSysfsGpioIO
                     (withPin testPin1 $ const $
                        throwM $ userError "Foo")
                     `shouldThrow` anyIOException
                   doesDirectoryExist "/sys/class/gpio/gpio48" `shouldReturn` False
              it "handles double-open and double-close gracefully" $
                do runSysfsGpioIO
                     (withPin testPin1 $ const $
                        withPin testPin1 $ const $
                          liftIO $ doesDirectoryExist "/sys/class/gpio/gpio48")
                     `shouldReturn` True
                   doesDirectoryExist "/sys/class/gpio/gpio48" `shouldReturn` False
              it "fails if the pin is invalid" $
                do runSysfsGpioIO
                     (withPin invalidPin $ const $
                        liftIO $ doesDirectoryExist "/sys/class/gpio/gpio9000")
                     `shouldThrow` isInvalidPinError
         context "getPinDirection/setPinDirection" $
           do it "gets and sets the pin's direction" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h ->
                    do udevScriptWait
                       setPinDirection h In
                       dir1 <- getPinDirection h
                       setPinDirection h Out
                       dir2 <- getPinDirection h
                       setPinDirection h In
                       dir3 <- getPinDirection h
                       return (dir1, dir2, dir3))
                  `shouldReturn` (Just In, Just Out, Just In)
              it "can set the pin's direction to 'Out' when it's configured for edge-triggered reads" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h ->
                    do udevScriptWait
                       setPinDirection h In
                       setPinInterruptMode h RisingEdge
                       setPinDirection h Out
                       setPinDirection h In
                       setPinInterruptMode h FallingEdge
                       setPinDirection h Out
                       setPinDirection h In
                       setPinInterruptMode h Level
                       setPinDirection h Out
                       return True)
                  `shouldReturn` True
         context "togglePinDirection" $
           do it "toggles the pin's direction" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h ->
                    do udevScriptWait
                       setPinDirection h In
                       dir1 <- togglePinDirection h
                       dir2 <- togglePinDirection h
                       dir3 <- togglePinDirection h
                       return (dir1, dir2, dir3))
                  `shouldReturn` (Just Out, Just In, Just Out)
              it "can set the pin's direction to 'Out' when it's configured for edge-triggered reads" $
                 runSysfsGpioIO
                   (withPin testPin1 $ \h ->
                     do udevScriptWait
                        setPinDirection h In
                        setPinInterruptMode h RisingEdge
                        dir1 <- togglePinDirection h
                        setPinDirection h In
                        setPinInterruptMode h FallingEdge
                        dir2 <- togglePinDirection h
                        setPinDirection h In
                        setPinInterruptMode h Level
                        dir3 <- togglePinDirection h
                        return (dir1, dir2, dir3))
                   `shouldReturn` (Just Out, Just Out, Just Out)
         context "getPinActiveLevel/setPinActiveLevel" $
           it "gets and sets the pin's active level" $
             runSysfsGpioIO
               (withPin testPin1 $ \h ->
                 do udevScriptWait
                    setPinActiveLevel h Low
                    level1 <- getPinActiveLevel h
                    setPinActiveLevel h High
                    level2 <- getPinActiveLevel h
                    setPinActiveLevel h Low
                    level3 <- getPinActiveLevel h
                    return (level1, level2, level3))
               `shouldReturn` (Low, High, Low)
         context "togglePinActiveLevel" $
           it "toggles the pin's active level" $
             runSysfsGpioIO
               (withPin testPin1 $ \h ->
                 do udevScriptWait
                    setPinActiveLevel h High
                    level1 <- togglePinActiveLevel h
                    level2 <- togglePinActiveLevel h
                    level3 <- togglePinActiveLevel h
                    return (level1, level2, level3))
               `shouldReturn` (Low, High, Low)
         context "readPin/writePin" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "reads and writes the pin's value" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do udevScriptWait
                          setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 Out
                          setPinActiveLevel h2 High
                          writePin h2 High
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          val1 <- readPin h1
                          writePin h2 Low
                          liftIO $ threadDelay 250000
                          val2 <- readPin h1
                          writePin h2 High
                          liftIO $ threadDelay 250000
                          val3 <- readPin h1
                          return (val1, val2, val3))
                  `shouldReturn` (High, Low, High)
              it "readPin obeys the pin's active level" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do udevScriptWait
                          setPinDirection h1 In
                          setPinActiveLevel h1 Low
                          setPinDirection h2 Out
                          setPinActiveLevel h2 High
                          writePin h2 High
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          val1 <- readPin h1
                          writePin h2 Low
                          liftIO $ threadDelay 250000
                          val2 <- readPin h1
                          writePin h2 High
                          liftIO $ threadDelay 250000
                          val3 <- readPin h1
                          return (val1, val2, val3))
                  `shouldReturn` (Low, High, Low)
              it "readPin works on output pins" $
                 runSysfsGpioIO
                   (withPin testPin2 $ \h ->
                      do udevScriptWait
                         setPinDirection h Out
                         setPinActiveLevel h High
                         writePin h High
                         liftIO $ threadDelay 250000
                         val1 <- readPin h
                         writePin h Low
                         liftIO $ threadDelay 250000
                         val2 <- readPin h
                         writePin h High
                         liftIO $ threadDelay 250000
                         val3 <- readPin h
                         return (val1, val2, val3))
                   `shouldReturn` (High, Low, High)
              it "readPin works on output pins (active-low)" $
                 runSysfsGpioIO
                   (withPin testPin2 $ \h ->
                      do udevScriptWait
                         setPinDirection h Out
                         setPinActiveLevel h Low
                         writePin h High
                         liftIO $ threadDelay 250000
                         val1 <- readPin h
                         writePin h Low
                         liftIO $ threadDelay 250000
                         val2 <- readPin h
                         writePin h High
                         liftIO $ threadDelay 250000
                         val3 <- readPin h
                         return (val1, val2, val3))
                   `shouldReturn` (High, Low, High)
              it "writePin obeys the pin's active level" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do udevScriptWait
                          setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 Out
                          setPinActiveLevel h2 Low
                          writePin h2 High
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          val1 <- readPin h1
                          writePin h2 Low
                          liftIO $ threadDelay 250000
                          val2 <- readPin h1
                          writePin h2 High
                          liftIO $ threadDelay 250000
                          val3 <- readPin h1
                          return (val1, val2, val3))
                  `shouldReturn` (Low, High, Low)
              it "fails if the pin is configured for input" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h ->
                     do udevScriptWait
                        setPinDirection h In
                        writePin h High)
                  `shouldThrow` isPermissionDeniedError
         context "writePin'" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "writes the pin's value and configures it for output simultaneously" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do udevScriptWait
                          setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 In
                          setPinActiveLevel h2 High
                          writePin' h2 High
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          val1 <- readPin h1
                          writePin' h2 Low
                          liftIO $ threadDelay 250000
                          val2 <- readPin h1
                          writePin' h2 High
                          liftIO $ threadDelay 250000
                          val3 <- readPin h1
                          return (val1, val2, val3))
                  `shouldReturn` (High, Low, High)
              it "works when the pin is configured for input and edge- or level-triggered reads" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do udevScriptWait
                          setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 In
                          setPinActiveLevel h2 High
                          setPinInterruptMode h2 RisingEdge
                          writePin' h2 High
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          val1 <- readPin h1
                          setPinDirection h2 In
                          setPinActiveLevel h2 High
                          setPinInterruptMode h2 FallingEdge
                          writePin' h2 Low
                          liftIO $ threadDelay 250000
                          val2 <- readPin h1
                          setPinDirection h2 In
                          setPinActiveLevel h2 High
                          setPinInterruptMode h2 Level
                          writePin' h2 High
                          liftIO $ threadDelay 250000
                          val3 <- readPin h1
                          return (val1, val2, val3))
                  `shouldReturn` (High, Low, High)
              it "writePin' obeys the pin's active level" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do udevScriptWait
                          setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 In
                          setPinActiveLevel h2 Low
                          writePin' h2 High
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          val1 <- readPin h1
                          writePin' h2 Low
                          liftIO $ threadDelay 250000
                          val2 <- readPin h1
                          writePin' h2 High
                          liftIO $ threadDelay 250000
                          val3 <- readPin h1
                          return (val1, val2, val3))
                  `shouldReturn` (Low, High, Low)
         context "togglePinValue" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "toggles the pin's value and returns the previous value" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do udevScriptWait
                          setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 Out
                          setPinActiveLevel h2 High
                          h2_val1 <- togglePinValue h2
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          h1_val1 <- readPin h1
                          h2_val2 <- togglePinValue h2
                          liftIO $ threadDelay 250000
                          h1_val2 <- readPin h1
                          h2_val3 <- togglePinValue h2
                          liftIO $ threadDelay 250000
                          h1_val3 <- readPin h1
                          return (h2_val1, h1_val1, h2_val2, h1_val2, h2_val3, h1_val3))
                  `shouldReturn` (High, High, Low, Low, High, High)
              it "togglePinValue obeys the pin's active level" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do udevScriptWait
                          setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 Out
                          setPinActiveLevel h2 Low
                          writePin h2 Low
                          h2_val1 <- togglePinValue h2
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          h1_val1 <- readPin h1
                          h2_val2 <- togglePinValue h2
                          liftIO $ threadDelay 250000
                          h1_val2 <- readPin h1
                          h2_val3 <- togglePinValue h2
                          liftIO $ threadDelay 250000
                          h1_val3 <- readPin h1
                          return (h2_val1, h1_val1, h2_val2, h1_val2, h2_val3, h1_val3))
                  `shouldReturn` (High, Low, Low, High, High, Low)
              it "fails if the pin is configured for input" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h ->
                     do udevScriptWait
                        setPinDirection h In
                        void $ togglePinValue h)
                  `shouldThrow` isPermissionDeniedError
         context "getPinInterruptMode/setPinInterruptMode" $
           do it "gets and sets the pin's interrupt mode" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h ->
                    do udevScriptWait
                       setPinDirection h In
                       setPinInterruptMode h RisingEdge
                       trigger1 <- getPinInterruptMode h
                       setPinInterruptMode h FallingEdge
                       trigger2 <- getPinInterruptMode h
                       setPinInterruptMode h Level
                       trigger3 <- getPinInterruptMode h
                       setPinInterruptMode h Disabled
                       trigger4 <- getPinInterruptMode h
                       return (trigger1, trigger2, trigger3, trigger4))
                `shouldReturn` (Just RisingEdge, Just FallingEdge, Just Level, Just Disabled)
              it "setPinInterruptMode should fail on an output pin" $
                runSysfsGpioIO
                  (withPin testPin2 $ \h ->
                    do udevScriptWait
                       setPinDirection h Out
                       setPinInterruptMode h Level)
                `shouldThrow` isInvalidOperationError
         context "pollPin" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "pollPin waits for rising edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h High
                            setPinInterruptMode h RisingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- pollPin h
                            val2 <- pollPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (High, High)
              it "pollPin waits for falling edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h High
                            setPinInterruptMode h FallingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- pollPin h
                            val2 <- pollPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Low, Low)
              it "pollPin waits for level changes" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h High
                            setPinInterruptMode h Level
                            liftIO $ putMVar mvar ()
                            val1 <- pollPin h
                            val2 <- pollPin h
                            val3 <- pollPin h
                            val4 <- pollPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2, val3, val4))
                   `shouldReturn` (Low, High, Low, High)
              it "pollPin can be disabled" $
                do mvar <- liftIO $ newEmptyMVar
                   runSysfsGpioIO
                     (withPin testPin1 $ \inPin ->
                        withPin testPin2 $ \outPin ->
                          do udevScriptWait
                             setPinDirection inPin In
                             setPinActiveLevel inPin High
                             setPinInterruptMode inPin Disabled
                             setPinDirection outPin Out
                             setPinActiveLevel outPin High
                             writePin outPin Low
                             void $ liftIO $ forkIO $
                               do runSysfsGpioIO $
                                    do liftIO $ void $ takeMVar mvar
                                       liftIO $ threadDelay 500000
                                       void $ togglePinValue outPin -- ignored
                                       liftIO $ threadDelay 500000
                                       setPinInterruptMode inPin Level
                                       void $ togglePinValue outPin -- trigger
                                  putMVar mvar () -- synchronize finish
                             liftIO $ putMVar mvar ()
                             val <- pollPin inPin
                             liftIO $ void $ takeMVar mvar -- synchronize finish
                             return val)
                   `shouldReturn` Low
         context "pollPin with active-low logic" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "pollPin waits for (active-low) rising edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h Low
                            setPinInterruptMode h RisingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- pollPin h
                            val2 <- pollPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (High, High)
              it "pollPin waits for (active-low) falling edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h Low
                            setPinInterruptMode h FallingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- pollPin h
                            val2 <- pollPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Low, Low)
              it "pollPin waits for level changes" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h Low
                            setPinInterruptMode h Level
                            liftIO $ putMVar mvar ()
                            val1 <- pollPin h
                            val2 <- pollPin h
                            val3 <- pollPin h
                            val4 <- pollPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2, val3, val4))
                   `shouldReturn` (Low, High, Low, High)
              it "pollPin can be disabled" $
                do mvar <- liftIO $ newEmptyMVar
                   runSysfsGpioIO
                     (withPin testPin1 $ \inPin ->
                        withPin testPin2 $ \outPin ->
                          do udevScriptWait
                             setPinDirection inPin In
                             setPinActiveLevel inPin Low
                             setPinInterruptMode inPin Disabled
                             setPinDirection outPin Out
                             setPinActiveLevel outPin High
                             writePin outPin Low
                             void $ liftIO $ forkIO $
                               do runSysfsGpioIO $
                                    do liftIO $ void $ takeMVar mvar
                                       liftIO $ threadDelay 500000
                                       void $ togglePinValue outPin -- ignored
                                       liftIO $ threadDelay 500000
                                       setPinInterruptMode inPin Level
                                       void $ togglePinValue outPin -- trigger
                                  putMVar mvar () -- synchronize finish
                             liftIO $ putMVar mvar ()
                             val <- pollPin inPin
                             liftIO $ void $ takeMVar mvar -- synchronize finish
                             return val)
                   `shouldReturn` High
         context "pollPinTimeout" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "pollPinTimeout waits for rising edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h High
                            setPinInterruptMode h RisingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- pollPinTimeout h 10000000
                            val2 <- pollPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Just High, Just High)
              it "pollPinTimeout waits for falling edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h High
                            setPinInterruptMode h FallingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- pollPinTimeout h 10000000
                            val2 <- pollPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Just Low, Just Low)
              it "pollPinTimeout waits for level changes" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h High
                            setPinInterruptMode h Level
                            liftIO $ putMVar mvar ()
                            val1 <- pollPinTimeout h 10000000
                            val2 <- pollPinTimeout h 10000000
                            val3 <- pollPinTimeout h 10000000
                            val4 <- pollPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2, val3, val4))
                   `shouldReturn` (Just Low, Just High, Just Low, Just High)
              it "pollPinTimeout can be disabled" $
                do mvar <- liftIO $ newEmptyMVar
                   runSysfsGpioIO
                     (withPin testPin1 $ \inPin ->
                        withPin testPin2 $ \outPin ->
                          do udevScriptWait
                             setPinDirection inPin In
                             setPinActiveLevel inPin High
                             setPinInterruptMode inPin Disabled
                             setPinDirection outPin Out
                             setPinActiveLevel outPin High
                             writePin outPin Low
                             void $ liftIO $ forkIO $
                               do runSysfsGpioIO $
                                    do liftIO $ void $ takeMVar mvar
                                       liftIO $ threadDelay 500000
                                       void $ togglePinValue outPin -- ignored
                                       liftIO $ threadDelay 500000
                                       setPinInterruptMode inPin Level
                                       void $ togglePinValue outPin -- trigger
                                  putMVar mvar () -- synchronize finish
                             liftIO $ putMVar mvar ()
                             val <- pollPinTimeout inPin 10000000
                             liftIO $ void $ takeMVar mvar -- synchronize finish
                             return val)
                   `shouldReturn` Just Low
              it "pollPinTimeout actually times out" $
                 do mvar <- liftIO $ newEmptyMVar
                    runSysfsGpioIO
                      (withPin testPin1 $ \inPin ->
                         withPin testPin2 $ \outPin ->
                           do udevScriptWait
                              setPinDirection inPin In
                              setPinActiveLevel inPin High
                              setPinInterruptMode inPin Disabled
                              setPinDirection outPin Out
                              setPinActiveLevel outPin High
                              writePin outPin Low
                              void $ liftIO $ forkIO $
                                do runSysfsGpioIO $
                                     do liftIO $ void $ takeMVar mvar
                                        liftIO $ threadDelay 500000
                                        void $ togglePinValue outPin -- ignored
                                        liftIO $ threadDelay 500000
                                        void $ togglePinValue outPin -- ignored
                                   putMVar mvar () -- synchronize finish
                              liftIO $ putMVar mvar ()
                              val <- pollPinTimeout inPin 1000000
                              liftIO $ void $ takeMVar mvar -- synchronize finish
                              return val)
                    `shouldReturn` Nothing
              it "pollPinTimeout times out on output pins" $
                 do runSysfsGpioIO
                      (withPin testPin2 $ \outPin ->
                        do udevScriptWait
                           setPinDirection outPin Out
                           setPinActiveLevel outPin High
                           val <- pollPinTimeout outPin 1000000
                           return val)
                    `shouldReturn` Nothing
         context "pollPinTimeout with active-low logic" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "pollPinTimeout waits for (active-low) rising edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h Low
                            setPinInterruptMode h RisingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- pollPinTimeout h 10000000
                            val2 <- pollPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Just High, Just High)
              it "pollPinTimeout waits for (active-low) falling edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h Low
                            setPinInterruptMode h FallingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- pollPinTimeout h 10000000
                            val2 <- pollPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Just Low, Just Low)
              it "pollPinTimeout waits for level changes" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do udevScriptWait
                               setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 500000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do udevScriptWait
                            setPinDirection h In
                            setPinActiveLevel h Low
                            setPinInterruptMode h Level
                            liftIO $ putMVar mvar ()
                            val1 <- pollPinTimeout h 10000000
                            val2 <- pollPinTimeout h 10000000
                            val3 <- pollPinTimeout h 10000000
                            val4 <- pollPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2, val3, val4))
                   `shouldReturn` (Just Low, Just High, Just Low, Just High)
              it "pollPinTimeout can be disabled" $
                do mvar <- liftIO $ newEmptyMVar
                   runSysfsGpioIO
                     (withPin testPin1 $ \inPin ->
                        withPin testPin2 $ \outPin ->
                          do udevScriptWait
                             setPinDirection inPin In
                             setPinActiveLevel inPin Low
                             setPinInterruptMode inPin Disabled
                             setPinDirection outPin Out
                             setPinActiveLevel outPin High
                             writePin outPin Low
                             void $ liftIO $ forkIO $
                               do runSysfsGpioIO $
                                    do liftIO $ void $ takeMVar mvar
                                       liftIO $ threadDelay 500000
                                       void $ togglePinValue outPin -- ignored
                                       liftIO $ threadDelay 500000
                                       setPinInterruptMode inPin Level
                                       void $ togglePinValue outPin -- trigger
                                  putMVar mvar () -- synchronize finish
                             liftIO $ putMVar mvar ()
                             val <- pollPinTimeout inPin 10000000
                             liftIO $ void $ takeMVar mvar -- synchronize finish
                             return val)
                   `shouldReturn` Just High
         context "Various NotExported exceptions" $
           do it "getPinDirection" $
                runSysfsGpioIO
                  (do h <- openPin testPin1
                      closePin h
                      v <- getPinDirection h
                      return v)
                  `shouldThrow` isNotExportedError
              it "setPinDirection" $
                 runSysfsGpioIO
                   (do h <- openPin testPin1
                       closePin h
                       setPinDirection h Out)
                   `shouldThrow` isNotExportedError
              it "togglePinDirection" $
                 runSysfsGpioIO
                   (do h <- openPin testPin1
                       closePin h
                       v <- togglePinDirection h
                       return v)
                   `shouldThrow` isNotExportedError
              it "getPinInterruptMode" $
                runSysfsGpioIO
                  (do h <- openPin testPin1
                      closePin h
                      v <- getPinInterruptMode h
                      return v)
                  `shouldThrow` isNotExportedError
              it "setPinInterruptMode" $
                 runSysfsGpioIO
                   (do h <- openPin testPin1
                       closePin h
                       setPinInterruptMode h RisingEdge)
                   `shouldThrow` isNotExportedError
              it "getPinActiveLevel" $
                runSysfsGpioIO
                  (do h <- openPin testPin1
                      closePin h
                      v <- getPinActiveLevel h
                      return v)
                  `shouldThrow` isNotExportedError
              it "setPinActiveLevel" $
                runSysfsGpioIO
                  (do h <- openPin testPin1
                      closePin h
                      setPinActiveLevel h High)
                  `shouldThrow` isNotExportedError
              it "toggleActiveLevel" $
                 runSysfsGpioIO
                   (do h <- openPin testPin1
                       closePin h
                       v <- togglePinActiveLevel h
                       return v)
                   `shouldThrow` isNotExportedError
              it "readPin" $
                runSysfsGpioIO
                  (do h <- openPin testPin1
                      closePin h
                      v <- readPin h
                      return v)
                  `shouldThrow` isNotExportedError
              it "pollPin" $
                runSysfsGpioIO
                  (do h <- openPin testPin1
                      closePin h
                      v <- pollPin h
                      return v)
                  `shouldThrow` isNotExportedError
              it "pollPinTimeout" $
                runSysfsGpioIO
                  (do h <- openPin testPin1
                      closePin h
                      v <- pollPinTimeout h 100000
                      return v)
                  `shouldThrow` isNotExportedError
              it "writePin" $
                runSysfsGpioIO
                  (do h <- openPin testPin1
                      closePin h
                      writePin h High)
                  `shouldThrow` isNotExportedError
              it "writePin'" $
                runSysfsGpioIO
                  (do h <- openPin testPin1
                      closePin h
                      writePin' h High)
                  `shouldThrow` isNotExportedError
              it "togglePinValue" $
                runSysfsGpioIO
                  (do h <- openPin testPin1
                      closePin h
                      v <- togglePinValue h
                      return v)
                  `shouldThrow` isNotExportedError
