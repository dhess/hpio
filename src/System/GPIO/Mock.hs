{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Mock
       ( Env(..)
       , runMock
       , runMockT
       ) where

import Control.Monad.RWS (MonadRWS, RWS, asks, evalRWS, tell)
import Control.Monad.Trans.Free (iterT)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T (intercalate, pack)
import System.GPIO.Free

data Env = Env { pins :: Set Pin } deriving (Show)

type MonadMock = MonadRWS Env [Text] ()

type Mock = RWS Env [Text] ()

tshow :: (Show a) => a -> Text
tshow = T.pack . show

runMockT :: (MonadMock m) => GpioT m a -> m a
runMockT = iterT run
  where
    run :: (MonadMock m) => GpioF (m a) -> m a

    run (Open p next) =
      do valid <- pinExists p
         if valid
            then
              do tell $ [T.intercalate " " ["Open", tshow p]]
                 next (Just $ PinDescriptor p)
            else
              do tell $ [T.intercalate " " ["Open failed:", tshow p, "does not exist"]]
                 next Nothing

    run (Close d next) =
      do tell $ [T.intercalate " " ["Close", tshow d]]
         next

    pinExists :: (MonadMock m) => Pin -> m Bool
    pinExists p = asks pins >>= return . (Set.member p)

runMock :: Env -> GpioT Mock a -> (a, [Text])
runMock env action = evalRWS (runMockT action) env ()
