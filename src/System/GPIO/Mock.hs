module System.GPIO.Mock
       ( runMock
       ) where

import Control.Monad.Free (iterM)
import Control.Monad.Writer (Writer, tell)
import System.GPIO.Free

runMock :: GpioM a -> Writer [String] a
runMock = iterM run
  where
    run :: GpioF (Writer [String] a) -> Writer [String] a

    run (AllocPin p next) =
      do tell $ ["Alloc " ++ show p]
         next

    run (DeallocPin p next) =
      do tell $ ["Dealloc " ++ show p]
         next
