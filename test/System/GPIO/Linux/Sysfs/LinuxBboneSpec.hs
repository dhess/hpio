{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module System.GPIO.Linux.Sysfs.LinuxBboneSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesDirectoryExist)
import System.GPIO.Linux.Sysfs (runSysfsGpioIO)
import System.GPIO.Monad (MonadGpio(..), withPin)
import System.GPIO.Types (Pin(..), PinDirection(..), PinReadTrigger(..), PinValue(..))
import Test.Hspec

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

runTests :: Spec
runTests =
  do
    describe "runSysfsGpioIO" $
      do context "pins" $
           it "returns the full list of pins on the system" $
             runSysfsGpioIO pins `shouldReturn` (map Pin [0..127])
         context "openPin/closePin" $
           it "exports/unexports the pin" $
             runSysfsGpioIO
               (do h <- openPin testPin1
                   exported <- liftIO $ doesDirectoryExist "/sys/class/gpio/gpio48"
                   closePin h
                   stillExported <- liftIO $ doesDirectoryExist "/sys/class/gpio/gpio48"
                   return (exported, stillExported))
               `shouldReturn` (True, False)
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
         context "getPinDirection/setPinDirection" $
           it "gets and sets the pin's direction" $
             runSysfsGpioIO
               (withPin testPin1 $ \h ->
                 do setPinDirection h In
                    dir1 <- getPinDirection h
                    setPinDirection h Out
                    dir2 <- getPinDirection h
                    setPinDirection h In
                    dir3 <- getPinDirection h
                    return (dir1, dir2, dir3))
               `shouldReturn` (Just In, Just Out, Just In)
         context "togglePinDirection" $
           it "toggles the pin's direction" $
             runSysfsGpioIO
               (withPin testPin1 $ \h ->
                 do setPinDirection h In
                    dir1 <- togglePinDirection h
                    dir2 <- togglePinDirection h
                    dir3 <- togglePinDirection h
                    return (dir1, dir2, dir3))
               `shouldReturn` (Just Out, Just In, Just Out)
         context "getPinActiveLevel/setPinActiveLevel" $
           it "gets and sets the pin's active level" $
             runSysfsGpioIO
               (withPin testPin1 $ \h ->
                 do setPinActiveLevel h Low
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
                 do setPinActiveLevel h High
                    level1 <- togglePinActiveLevel h
                    level2 <- togglePinActiveLevel h
                    level3 <- togglePinActiveLevel h
                    return (level1, level2, level3))
               `shouldReturn` (Low, High, Low)
         context "samplePin/writePin" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "reads and writes the pin's value" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 Out
                          setPinActiveLevel h2 High
                          writePin h2 High
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          val1 <- samplePin h1
                          writePin h2 Low
                          liftIO $ threadDelay 250000
                          val2 <- samplePin h1
                          writePin h2 High
                          liftIO $ threadDelay 250000
                          val3 <- samplePin h1
                          return (val1, val2, val3))
                  `shouldReturn` (High, Low, High)
              it "samplePin obeys the pin's active level" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do setPinDirection h1 In
                          setPinActiveLevel h1 Low
                          setPinDirection h2 Out
                          setPinActiveLevel h2 High
                          writePin h2 High
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          val1 <- samplePin h1
                          writePin h2 Low
                          liftIO $ threadDelay 250000
                          val2 <- samplePin h1
                          writePin h2 High
                          liftIO $ threadDelay 250000
                          val3 <- samplePin h1
                          return (val1, val2, val3))
                  `shouldReturn` (Low, High, Low)
              it "writePin obeys the pin's active level" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 Out
                          setPinActiveLevel h2 Low
                          writePin h2 High
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          val1 <- samplePin h1
                          writePin h2 Low
                          liftIO $ threadDelay 250000
                          val2 <- samplePin h1
                          writePin h2 High
                          liftIO $ threadDelay 250000
                          val3 <- samplePin h1
                          return (val1, val2, val3))
                  `shouldReturn` (Low, High, Low)
         context "writePin'" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "writes the pin's value and configures it for output simultaneously" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 In
                          setPinActiveLevel h2 High
                          writePin' h2 High
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          val1 <- samplePin h1
                          writePin' h2 Low
                          liftIO $ threadDelay 250000
                          val2 <- samplePin h1
                          writePin' h2 High
                          liftIO $ threadDelay 250000
                          val3 <- samplePin h1
                          return (val1, val2, val3))
                  `shouldReturn` (High, Low, High)
              it "writePin' obeys the pin's active level" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 In
                          setPinActiveLevel h2 Low
                          writePin' h2 High
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          val1 <- samplePin h1
                          writePin' h2 Low
                          liftIO $ threadDelay 250000
                          val2 <- samplePin h1
                          writePin' h2 High
                          liftIO $ threadDelay 250000
                          val3 <- samplePin h1
                          return (val1, val2, val3))
                  `shouldReturn` (Low, High, Low)
         context "togglePinValue" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "toggles the pin's value and returns the previous value" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 Out
                          setPinActiveLevel h2 High
                          h2_val1 <- togglePinValue h2
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          h1_val1 <- samplePin h1
                          h2_val2 <- togglePinValue h2
                          liftIO $ threadDelay 250000
                          h1_val2 <- samplePin h1
                          h2_val3 <- togglePinValue h2
                          liftIO $ threadDelay 250000
                          h1_val3 <- samplePin h1
                          return (h2_val1, h1_val1, h2_val2, h1_val2, h2_val3, h1_val3))
                  `shouldReturn` (High, High, Low, Low, High, High)
              it "togglePinValue obeys the pin's active level" $
                runSysfsGpioIO
                  (withPin testPin1 $ \h1 ->
                     withPin testPin2 $ \h2 ->
                       do setPinDirection h1 In
                          setPinActiveLevel h1 High
                          setPinDirection h2 Out
                          setPinActiveLevel h2 Low
                          writePin h2 Low
                          h2_val1 <- togglePinValue h2
                          -- give the pin time to settle
                          liftIO $ threadDelay 250000
                          h1_val1 <- samplePin h1
                          h2_val2 <- togglePinValue h2
                          liftIO $ threadDelay 250000
                          h1_val2 <- samplePin h1
                          h2_val3 <- togglePinValue h2
                          liftIO $ threadDelay 250000
                          h1_val3 <- samplePin h1
                          return (h2_val1, h1_val1, h2_val2, h1_val2, h2_val3, h1_val3))
                  `shouldReturn` (High, Low, Low, High, High, Low)
         context "getPinReadTrigger/setPinReadTrigger" $
           it "gets and sets the pin's read trigger" $
             runSysfsGpioIO
               (withPin testPin1 $ \h ->
                 do setPinReadTrigger h RisingEdge
                    trigger1 <- getPinReadTrigger h
                    setPinReadTrigger h FallingEdge
                    trigger2 <- getPinReadTrigger h
                    setPinReadTrigger h Level
                    trigger3 <- getPinReadTrigger h
                    setPinReadTrigger h Disabled
                    trigger4 <- getPinReadTrigger h
                    return (trigger1, trigger2, trigger3, trigger4))
               `shouldReturn` (Just RisingEdge, Just FallingEdge, Just Level, Just Disabled)
         context "readPin" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "readPin waits for rising edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h High
                            setPinReadTrigger h RisingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- readPin h
                            val2 <- readPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (High, High)
              it "readPin waits for falling edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h High
                            setPinReadTrigger h FallingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- readPin h
                            val2 <- readPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Low, Low)
              it "readPin waits for level changes" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h High
                            setPinReadTrigger h Level
                            liftIO $ putMVar mvar ()
                            val1 <- readPin h
                            val2 <- readPin h
                            val3 <- readPin h
                            val4 <- readPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2, val3, val4))
                   `shouldReturn` (Low, High, Low, High)
              it "readPin can be disabled" $
                do mvar <- liftIO $ newEmptyMVar
                   runSysfsGpioIO
                     (withPin testPin1 $ \inPin ->
                        withPin testPin2 $ \outPin ->
                          do setPinDirection inPin In
                             setPinActiveLevel inPin High
                             setPinReadTrigger inPin Disabled
                             setPinDirection outPin Out
                             setPinActiveLevel outPin High
                             writePin outPin Low
                             void $ liftIO $ forkIO $
                               do runSysfsGpioIO $
                                    do liftIO $ void $ takeMVar mvar
                                       liftIO $ threadDelay 250000
                                       void $ togglePinValue outPin -- ignored
                                       liftIO $ threadDelay 250000
                                       setPinReadTrigger inPin Level
                                       void $ togglePinValue outPin -- trigger
                                  putMVar mvar () -- synchronize finish
                             liftIO $ putMVar mvar ()
                             val <- readPin inPin
                             liftIO $ void $ takeMVar mvar -- synchronize finish
                             return val)
                   `shouldReturn` Low
         context "readPin with active-low logic" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "readPin waits for (active-low) rising edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h Low
                            setPinReadTrigger h RisingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- readPin h
                            val2 <- readPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (High, High)
              it "readPin waits for (active-low) falling edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h Low
                            setPinReadTrigger h FallingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- readPin h
                            val2 <- readPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Low, Low)
              it "readPin waits for level changes" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h Low
                            setPinReadTrigger h Level
                            liftIO $ putMVar mvar ()
                            val1 <- readPin h
                            val2 <- readPin h
                            val3 <- readPin h
                            val4 <- readPin h
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2, val3, val4))
                   `shouldReturn` (Low, High, Low, High)
              it "readPin can be disabled" $
                do mvar <- liftIO $ newEmptyMVar
                   runSysfsGpioIO
                     (withPin testPin1 $ \inPin ->
                        withPin testPin2 $ \outPin ->
                          do setPinDirection inPin In
                             setPinActiveLevel inPin Low
                             setPinReadTrigger inPin Disabled
                             setPinDirection outPin Out
                             setPinActiveLevel outPin High
                             writePin outPin Low
                             void $ liftIO $ forkIO $
                               do runSysfsGpioIO $
                                    do liftIO $ void $ takeMVar mvar
                                       liftIO $ threadDelay 250000
                                       void $ togglePinValue outPin -- ignored
                                       liftIO $ threadDelay 250000
                                       setPinReadTrigger inPin Level
                                       void $ togglePinValue outPin -- trigger
                                  putMVar mvar () -- synchronize finish
                             liftIO $ putMVar mvar ()
                             val <- readPin inPin
                             liftIO $ void $ takeMVar mvar -- synchronize finish
                             return val)
                   `shouldReturn` High
         context "readPinTimeout" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "readPinTimeout waits for rising edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h High
                            setPinReadTrigger h RisingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- readPinTimeout h 10000000
                            val2 <- readPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Just High, Just High)
              it "readPinTimeout waits for falling edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h High
                            setPinReadTrigger h FallingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- readPinTimeout h 10000000
                            val2 <- readPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Just Low, Just Low)
              it "readPinTimeout waits for level changes" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h High
                            setPinReadTrigger h Level
                            liftIO $ putMVar mvar ()
                            val1 <- readPinTimeout h 10000000
                            val2 <- readPinTimeout h 10000000
                            val3 <- readPinTimeout h 10000000
                            val4 <- readPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2, val3, val4))
                   `shouldReturn` (Just Low, Just High, Just Low, Just High)
              it "readPinTimeout can be disabled" $
                do mvar <- liftIO $ newEmptyMVar
                   runSysfsGpioIO
                     (withPin testPin1 $ \inPin ->
                        withPin testPin2 $ \outPin ->
                          do setPinDirection inPin In
                             setPinActiveLevel inPin High
                             setPinReadTrigger inPin Disabled
                             setPinDirection outPin Out
                             setPinActiveLevel outPin High
                             writePin outPin Low
                             void $ liftIO $ forkIO $
                               do runSysfsGpioIO $
                                    do liftIO $ void $ takeMVar mvar
                                       liftIO $ threadDelay 250000
                                       void $ togglePinValue outPin -- ignored
                                       liftIO $ threadDelay 250000
                                       setPinReadTrigger inPin Level
                                       void $ togglePinValue outPin -- trigger
                                  putMVar mvar () -- synchronize finish
                             liftIO $ putMVar mvar ()
                             val <- readPinTimeout inPin 10000000
                             liftIO $ void $ takeMVar mvar -- synchronize finish
                             return val)
                   `shouldReturn` Just Low
              it "readPinTimeout actually times out" $
                 do mvar <- liftIO $ newEmptyMVar
                    runSysfsGpioIO
                      (withPin testPin1 $ \inPin ->
                         withPin testPin2 $ \outPin ->
                           do setPinDirection inPin In
                              setPinActiveLevel inPin High
                              setPinReadTrigger inPin Disabled
                              setPinDirection outPin Out
                              setPinActiveLevel outPin High
                              writePin outPin Low
                              void $ liftIO $ forkIO $
                                do runSysfsGpioIO $
                                     do liftIO $ void $ takeMVar mvar
                                        liftIO $ threadDelay 250000
                                        void $ togglePinValue outPin -- ignored
                                        liftIO $ threadDelay 250000
                                        void $ togglePinValue outPin -- ignored
                                   putMVar mvar () -- synchronize finish
                              liftIO $ putMVar mvar ()
                              val <- readPinTimeout inPin 1000000
                              liftIO $ void $ takeMVar mvar -- synchronize finish
                              return val)
                    `shouldReturn` Nothing
         context "readPinTimeout with active-low logic" $
           -- Note: if these tests fail, you might not have hooked pin
           -- P9-15 up to pin P8-15!
           do it "readPinTimeout waits for (active-low) rising edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h Low
                            setPinReadTrigger h RisingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- readPinTimeout h 10000000
                            val2 <- readPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Just High, Just High)
              it "readPinTimeout waits for (active-low) falling edge" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h High
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h Low
                            setPinReadTrigger h FallingEdge
                            liftIO $ putMVar mvar ()
                            val1 <- readPinTimeout h 10000000
                            val2 <- readPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2))
                   `shouldReturn` (Just Low, Just Low)
              it "readPinTimeout waits for level changes" $
                do mvar <- liftIO $ newEmptyMVar
                   void $ liftIO $ forkIO $
                     do runSysfsGpioIO $
                          withPin testPin2 $ \h ->
                            do setPinDirection h Out
                               setPinActiveLevel h High
                               writePin h Low
                               liftIO $ void $ takeMVar mvar
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                               liftIO $ threadDelay 250000
                               void $ togglePinValue h -- trigger
                        putMVar mvar () -- synchronize finish
                   runSysfsGpioIO
                      (withPin testPin1 $ \h ->
                         do setPinDirection h In
                            setPinActiveLevel h Low
                            setPinReadTrigger h Level
                            liftIO $ putMVar mvar ()
                            val1 <- readPinTimeout h 10000000
                            val2 <- readPinTimeout h 10000000
                            val3 <- readPinTimeout h 10000000
                            val4 <- readPinTimeout h 10000000
                            liftIO $ void $ takeMVar mvar -- synchronize finish
                            return (val1, val2, val3, val4))
                   `shouldReturn` (Just Low, Just High, Just Low, Just High)
              it "readPinTimeout can be disabled" $
                do mvar <- liftIO $ newEmptyMVar
                   runSysfsGpioIO
                     (withPin testPin1 $ \inPin ->
                        withPin testPin2 $ \outPin ->
                          do setPinDirection inPin In
                             setPinActiveLevel inPin Low
                             setPinReadTrigger inPin Disabled
                             setPinDirection outPin Out
                             setPinActiveLevel outPin High
                             writePin outPin Low
                             void $ liftIO $ forkIO $
                               do runSysfsGpioIO $
                                    do liftIO $ void $ takeMVar mvar
                                       liftIO $ threadDelay 250000
                                       void $ togglePinValue outPin -- ignored
                                       liftIO $ threadDelay 250000
                                       setPinReadTrigger inPin Level
                                       void $ togglePinValue outPin -- trigger
                                  putMVar mvar () -- synchronize finish
                             liftIO $ putMVar mvar ()
                             val <- readPinTimeout inPin 10000000
                             liftIO $ void $ takeMVar mvar -- synchronize finish
                             return val)
                   `shouldReturn` Just High
