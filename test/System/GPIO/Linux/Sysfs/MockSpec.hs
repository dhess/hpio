{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs.MockSpec (spec) where

import System.GPIO.Linux.Sysfs.Mock
import Test.Hspec

spec :: Spec
spec =
  let sysfsRootZ = (sysfsRoot, [])
  in
    do describe "SysfsMockT" $
         do context "doesDirectoryExist" $
              do it "is relative to the initial zipper's working directory" $
                   do (result1, _) <- runSysfsMockT (doesDirectoryExist "sys/class/gpio") sysfsRootZ
                      result1 `shouldBe` True
                      let Right z1 = cd "sys" sysfsRootZ
                      (result2, _) <- runSysfsMockT (doesDirectoryExist "sys/class/gpio") z1
                      result2 `shouldBe` False
                      (result3, _) <- runSysfsMockT (doesDirectoryExist "class/gpio") sysfsRootZ
                      result3 `shouldBe` False
                      (result4, _) <- runSysfsMockT (doesDirectoryExist "class/gpio") z1
                      result4 `shouldBe` True
                      -- Files
                      (result5, _) <- runSysfsMockT (doesDirectoryExist "sys/class/gpio/export") sysfsRootZ
                      result5 `shouldBe` False
                      (result6, _) <- runSysfsMockT (doesDirectoryExist "class/gpio/export") z1
                      result6 `shouldBe` False
                      -- Does not exist
                      (result7, _) <- runSysfsMockT (doesDirectoryExist "sys/class/foobar") sysfsRootZ
                      result7 `shouldBe` False
                      (result8, _) <- runSysfsMockT (doesDirectoryExist "class/foobar") z1
                      result8 `shouldBe` False
                 it "works with absolute paths regardless of the initial zipper's working directory" $
                   do (result1, _) <- runSysfsMockT (doesDirectoryExist "/sys/class/gpio") sysfsRootZ
                      result1 `shouldBe` True
                      let Right z1 = cd "/sys/class" sysfsRootZ
                      (result2, _) <- runSysfsMockT (doesDirectoryExist "/sys/class/gpio") z1
                      result2 `shouldBe` True
                      (result3, _) <- runSysfsMockT (doesDirectoryExist "/gpio") z1
                      result3 `shouldBe` False
                      -- Files
                      (result4, _) <- runSysfsMockT (doesDirectoryExist "/sys/class/gpio/export") sysfsRootZ
                      result4 `shouldBe` False
                      (result5, _) <- runSysfsMockT (doesDirectoryExist "/sys/class/gpio/export") z1
                      result5 `shouldBe` False
                      -- Does not exist
                      (result6, _) <- runSysfsMockT (doesDirectoryExist "/sys/class/foobar") sysfsRootZ
                      result6 `shouldBe` False
                      (result7, _) <- runSysfsMockT (doesDirectoryExist "/sys/class/foobar") z1
                      result7 `shouldBe` False
                 it "doesn't change the zipper's state" $
                   do (_, rz1) <- runSysfsMockT (doesDirectoryExist "sys/class/gpio") sysfsRootZ
                      rz1 `shouldBe` sysfsRootZ
                      let Right z2 = cd "sys" sysfsRootZ
                      (_, rz2) <- runSysfsMockT (doesDirectoryExist "sys/class/gpio") z2
                      rz2 `shouldBe` z2
                      (_, rz3) <- runSysfsMockT (doesDirectoryExist "class/gpio") sysfsRootZ
                      rz3 `shouldBe` sysfsRootZ
                      (_, rz4) <- runSysfsMockT (doesDirectoryExist "class/gpio") z2
                      rz4 `shouldBe` z2
