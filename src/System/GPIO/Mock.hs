{-# LANGUAGE FlexibleContexts #-}

module System.GPIO.Mock
       ( runMock
       , runMockT
       ) where

import Control.Monad.RWS (MonadRWS, RWS, evalRWS, tell)
import Control.Monad.Trans.Free (iterT)
import System.GPIO.Free

runMockT :: (MonadRWS () [String] () m) => GpioT m a -> m a
runMockT = iterT run
  where
    run :: (MonadRWS () [String] () m) => GpioF (m a) -> m a

    run (Open p next) =
      do tell $ ["Open " ++ show p]
         next (Just $ PinDescriptor p)

    run (Close d next) =
     do tell $ ["Close " ++ show d]
        next

runMock :: GpioT (RWS () [String] ()) a -> (a, [String])
runMock action = evalRWS (runMockT action) () ()
