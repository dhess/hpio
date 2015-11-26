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

import Control.Error.Util (note)
import Control.Monad.Except
import Control.Monad.RWS (MonadRWS, RWS, asks, get, gets, put, runRWS, tell)
import Control.Monad.Trans.Free (iterT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T (intercalate, pack)
import System.GPIO.Free (GpioF(..), GpioT, Direction(..), Pin(..), PinDescriptor(..), Value(..))

data Env = Env { pins :: Set Pin } deriving (Show)

data PinState = PinState { direction :: !Direction, value :: !Value } deriving (Show, Eq)

type PinStateMap = Map PinDescriptor PinState

data World =
  World { pinStates :: PinStateMap }
  deriving (Show, Eq)

emptyWorld :: World
emptyWorld = World $ Map.empty

initialState :: PinState
initialState = PinState In Low

type MonadMock = MonadRWS Env [Text] World

type Mock = RWS Env [Text] World

tshow :: (Show a) => a -> Text
tshow = T.pack . show

runMockT :: (MonadError String m, MonadMock m) => GpioT m a -> m a
runMockT = iterT run
  where
    run :: (MonadError String m, MonadMock m) => GpioF (m a) -> m a

    run (Open p next) =
      do valid <- pinExists p
         case valid of
           False -> next (Left $ "Open failed: " ++ show p ++ " does not exist")
           True ->
             do say ["Open", tshow p]
                world <- get
                let d = PinDescriptor p
                put (World $ Map.insert d initialState (pinStates world))
                next (Right d)

    run (Close d next) =
      do valid <- validDescriptor d
         case valid of
           False -> next -- double-close is OK
           True ->
             do world <- get
                put (World $ Map.delete d (pinStates world))
                say ["Close", tshow d]
                next

    run (HasDirection _ next) = next True

    run (GetDirection d next) =
      do eitherDirection <- pinDirection d
         case eitherDirection of
           Left e -> throwError e
           Right d -> next d

    run (SetDirection d v next) =
      do eitherState <- pinState d
         case eitherState of
           Left e -> throwError e
           Right s ->
             do states <- gets pinStates
                put (World $ Map.insert d (s { direction = v }) states)
                next

    run (ReadPin d next) =
      do eitherValue <- pinValue d
         case eitherValue of
           Left e -> throwError e
           Right v -> next v

    run (WritePin d v next) =
      do eitherState <- pinState d
         case eitherState of
           Left e -> throwError e
           Right s ->
             case direction s of
               In -> throwError (show d ++ " is configured for input")
               Out ->
                 do states <- gets pinStates
                    put (World $ Map.insert d (s { value = v }) states)
                    next

pinExists :: (MonadMock m) => Pin -> m Bool
pinExists p = asks pins >>= return . (Set.member p)

validDescriptor :: (MonadMock m) => PinDescriptor -> m Bool
validDescriptor d = gets pinStates >>= return . (Map.member d)

say :: (MonadMock m) => [Text] -> m ()
say t = tell $ [T.intercalate " " t]

pinF :: (MonadMock m) => (PinState -> a) -> PinDescriptor -> m (Either String a)
pinF f d =
  do states <- gets pinStates
     return $ fmap f (note (show d ++ " is not a valid pin descriptor") (Map.lookup d states))

pinState :: (MonadMock m) => PinDescriptor -> m (Either String PinState)
pinState = pinF id

pinValue :: (MonadMock m) => PinDescriptor -> m (Either String Value)
pinValue = pinF value

pinDirection :: (MonadMock m) => PinDescriptor -> m (Either String Direction)
pinDirection = pinF direction

-- | Run a GpioT program in a pure environment mimicking IO;
-- exceptions are manifested as 'Either' 'String' 'a'.
runMock :: Env -> GpioT (ExceptT String Mock) a -> (Either String a, World, [Text])
runMock env action = runRWS (runExceptT $ runMockT action) env emptyWorld
