module Main where

import System.FilePath ((</>))
import Test.DocTest

addPrefix :: FilePath -> FilePath
addPrefix fp = "src" </> "System" </> "GPIO" </> fp

testFiles :: [FilePath]
testFiles =
  map addPrefix
      [ "Monad.hs"
      , "Tutorial.hs"
      , "Types.hs"
      , "Linux" </> "Sysfs" </> "Mock.hs"
      , "Linux" </> "Sysfs" </> "Mock" </> "Internal.hs"
      , "Linux" </> "Sysfs" </> "Monad.hs"
      , "Linux" </> "Sysfs" </> "Util.hs"
      , "Linux" </> "Sysfs" </> "Types.hs"
      ]

main :: IO ()
main = doctest (mappend ["-isrc"] testFiles)
