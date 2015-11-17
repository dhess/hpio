{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module System.GPIO.Mock
       ( runMock
       , runMockT
       ) where

import Control.Monad.RWS (MonadRWS, RWS, evalRWS, tell)
import Control.Monad.Trans.Free (iterT)
import System.GPIO.Free

type MonadMock = MonadRWS () [String] ()

type Mock = RWS () [String] ()

runMockT :: (MonadMock m) => GpioT m a -> m a
runMockT = iterT run
  where
    run :: (MonadMock m) => GpioF (m a) -> m a

    run (Open p next) =
      do tell $ ["Open " ++ show p]
         next (Just $ PinDescriptor p)

    run (Close d next) =
     do tell $ ["Close " ++ show d]
        next

runMock :: GpioT Mock a -> (a, [String])
runMock action = evalRWS (runMockT action) () ()
