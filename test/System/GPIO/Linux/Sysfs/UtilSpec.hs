{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.UtilSpec (spec) where

import System.GPIO.Types (Pin(..))
import System.GPIO.Linux.Sysfs.Util
import Test.Hspec

spec :: Spec
spec =
  do describe "sysfsPath" $
       it "is \"/sys/class/gpio\"" $
         sysfsPath `shouldBe` "/sys/class/gpio"

     describe "exportFileName" $
       it "is \"/sys/class/gpio/export\"" $
         exportFileName `shouldBe` "/sys/class/gpio/export"

     describe "unexportFileName" $
       it "is \"/sys/class/gpio/unexport\"" $
         unexportFileName `shouldBe` "/sys/class/gpio/unexport"

     describe "pinDirName" $
       it "is of the form \"/sys/class/gpio/gpioN\"" $
         pinDirName (Pin 16) `shouldBe` "/sys/class/gpio/gpio16"

     describe "pinActiveLowFileName" $
       it "is of the form \"/sys/class/gpio/gpioN/active_low\"" $
         pinActiveLowFileName (Pin 7) `shouldBe` "/sys/class/gpio/gpio7/active_low"

     describe "pinDirectionFileName" $
       it "is of the form \"/sys/class/gpio/gpioN/direction\"" $
         pinDirectionFileName (Pin 32) `shouldBe` "/sys/class/gpio/gpio32/direction"

     describe "pinEdgeFileName" $
       it "is of the form \"/sys/class/gpio/gpioN/edge\"" $
         pinEdgeFileName (Pin 8) `shouldBe` "/sys/class/gpio/gpio8/edge"

     describe "pinValueFileName" $
       it "is of the form \"/sys/class/gpio/gpioN/value\"" $
         pinValueFileName (Pin 0) `shouldBe` "/sys/class/gpio/gpio0/value"
