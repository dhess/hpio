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

    run (Open p next) =
      do tell $ ["Open " ++ show p]
         next (Just $ PinDescriptor p)

    run (Close d next) =
      do tell $ ["Close " ++ show d]
         next

runMock :: GpioT (Writer [String]) a -> Writer [String] a
runMock = runMockT
