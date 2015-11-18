{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Mock
       ( Env(..)
       , World(..)
       , emptyWorld
       , runMock
       , runMockT
       ) where

import Control.Monad.RWS (MonadRWS, RWS, asks, get, gets, put, runRWS, tell)
import Control.Monad.Trans.Free (iterT)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T (intercalate, pack)
import System.GPIO.Free

data Env = Env { pins :: Set Pin } deriving (Show)

data World = World { descriptors :: Set PinDescriptor } deriving (Show, Eq)

emptyWorld :: World
emptyWorld = World $ Set.empty

type MonadMock = MonadRWS Env [Text] World

type Mock = RWS Env [Text] World

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
              do say ["Open", tshow p]
                 world <- get
                 let d = PinDescriptor p
                 put (World $ Set.insert d (descriptors world))
                 next (Just d)
            else
              do say ["Open failed:", tshow p, "does not exist"]
                 next Nothing

    run (Close d next) =
      do valid <- validDescriptor d
         if valid
            then
              do world <- get
                 put (World $ Set.delete d (descriptors world))
                 say ["Close", tshow d]
                 next
            else
              do say ["Close failed:", tshow d, "is not a valid descriptor (double close?)"]
                 undefined -- Need an error here.
                 next

    pinExists :: (MonadMock m) => Pin -> m Bool
    pinExists p = asks pins >>= return . (Set.member p)

    validDescriptor :: (MonadMock m) => PinDescriptor -> m Bool
    validDescriptor d = gets descriptors >>= return . (Set.member d)

    say :: (MonadMock m) => [Text] -> m ()
    say t = tell $ [T.intercalate " " t]

runMock :: Env -> GpioT Mock a -> (a, World, [Text])
runMock env action = runRWS (runMockT action) env emptyWorld
