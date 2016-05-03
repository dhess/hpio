{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module System.GPIO.Linux.Sysfs.LinuxBboneSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesDirectoryExist)
import System.GPIO.Linux.Sysfs (runSysfsGpioIO)
import System.GPIO.Monad (MonadGpio(..), withPin)
import System.GPIO.Types (Pin(..), PinDirection(..), PinValue(..))
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
