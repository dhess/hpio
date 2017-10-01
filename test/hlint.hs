module Main where

import Protolude
import Language.Haskell.HLint

main :: IO ()
main =
  do args <- getArgs
     hints <- hlint $ ["src", "--cpp-define=HLINT", "--cpp-ansi"] ++ args
     unless (null hints) exitFailure
