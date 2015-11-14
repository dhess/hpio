{-# LANGUAGE FlexibleContexts #-}

module System.GPIO.Mock
       ( runMock
       , runMockT
       ) where

import Control.Monad.Trans.Free (iterT)
import Control.Monad.Writer (MonadWriter, Writer, tell)
import System.GPIO.Free

runMockT :: (MonadWriter [String] m) => GpioT m a -> m a
runMockT = iterT run
  where
    run :: (MonadWriter [String] m) => GpioF (m a) -> m a

    run (AllocPin p next) =
      do tell $ ["Alloc " ++ show p]
         next

    run (DeallocPin p next) =
      do tell $ ["Dealloc " ++ show p]
         next

runMock :: GpioT (Writer [String]) a -> Writer [String] a
runMock = runMockT
