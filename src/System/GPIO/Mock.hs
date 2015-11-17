{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Mock
       ( runMock
       , runMockT
       ) where

import Control.Monad.RWS (MonadRWS, RWS, evalRWS, tell)
import Control.Monad.Trans.Free (iterT)
import Data.Text (Text)
import qualified Data.Text as T (intercalate, pack)
import System.GPIO.Free

type MonadMock = MonadRWS () [Text] ()

type Mock = RWS () [Text] ()

tshow :: (Show a) => a -> Text
tshow = T.pack . show

runMockT :: (MonadMock m) => GpioT m a -> m a
runMockT = iterT run
  where
    run :: (MonadMock m) => GpioF (m a) -> m a

    run (Open p next) =
      do tell $ [T.intercalate " " ["Open", tshow p]]
         next (Just $ PinDescriptor p)

    run (Close d next) =
     do tell $ [T.intercalate " " ["Close", tshow d]]
        next

runMock :: GpioT Mock a -> (a, [Text])
runMock action = evalRWS (runMockT action) () ()
