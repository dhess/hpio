module Main where

import Protolude
import System.FilePath ((</>))
import Test.DocTest

addPrefix :: FilePath -> FilePath
addPrefix fp = "src" </> "System" </> "GPIO" </> fp

testFiles :: [FilePath]
testFiles =
  map addPrefix
      [ "Tutorial.hs"
      , "Types.hs"
      , "Linux" </> "Sysfs" </> "Mock.hs"
      , "Linux" </> "Sysfs" </> "Util.hs"
      , "Linux" </> "Sysfs" </> "Types.hs"
      ]

main :: IO ()
main = doctest (["-isrc", "-XNoImplicitPrelude"] <> testFiles)
