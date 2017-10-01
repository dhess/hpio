module Main where

import Protolude
import Control.Monad (unless)
import Language.Haskell.HLint

main :: IO ()
main =
  do args <- getArgs
     hints <- hlint $ ["src", "--cpp-define=HLINT", "--cpp-ansi"] ++ args
     unless (null hints) exitFailure
