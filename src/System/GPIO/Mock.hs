{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Mock
       ( AvailablePins
       , MockHandle(..)
       , MockF
       , MockT
       , MockState(..)
       , MockStateMap
       , evalMock
       , execMock
       , runMock
       , runMockT
       ) where

import Control.Error.Util (note)
import Control.Monad.Except
import Control.Monad.RWS (MonadRWS, RWS, ask, get, put, runRWS, tell)
import Control.Monad.Trans.Free (iterT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T (intercalate, pack)
import System.GPIO.Free (GpioF(..), GpioT, Pin(..), PinDirection(..), Value(..))

type AvailablePins = Set Pin

data MockState = MockState { dir :: !PinDirection, value :: !Value } deriving (Show, Eq)

data MockHandle = MockHandle Pin deriving (Show, Eq, Ord)

type MockStateMap = Map MockHandle MockState

initialState :: MockState
initialState = MockState In Low

type MonadMock = MonadRWS AvailablePins [Text] MockStateMap

type Mock = RWS AvailablePins [Text] MockStateMap

type MockT = GpioT String MockHandle

type MockF = GpioF String MockHandle

runMockT :: (MonadError String m, MonadMock m) => MockT m a -> m a
runMockT = iterT run
  where
    run :: (MonadError String m, MonadMock m) => MockF (m a) -> m a

    run (Open p next) =
      do valid <- pinExists p
         case valid of
           False -> next (Left $ "Open failed: " ++ show p ++ " does not exist")
           True ->
             do say ["Open", tshow p]
                states <- get
                let d = MockHandle p
                put (Map.insert d initialState states)
                next (Right d)

    run (Close h next) =
      do valid <- validHandle h
         case valid of
           False -> next -- double-close is OK
           True ->
             do states <- get
                put (Map.delete h states)
                say ["Close", tshow h]
                next

    run (Direction h next) =
      do void $ checkHandle h
         eitherDirection <- pinDirection h
         case eitherDirection of
           Left e -> throwError e
           Right dir' -> next (Just dir')

    run (SetDirection h v next) =
      do void $ checkHandle h
         eitherState <- mockState h
         case eitherState of
           Left e -> throwError e
           Right s ->
             do states <- get
                put (Map.insert h (s { dir = v }) states)
                say ["Set direction:", tshow h, tshow v ]
                next

    run (ReadPin h next) =
      do void $ checkHandle h
         eitherValue <- pinValue h
         case eitherValue of
           Left e -> throwError e
           Right v -> next v

    run (WritePin h v next) =
      do void $ checkHandle h
         eitherState <- mockState h
         case eitherState of
           Left e -> throwError e
           Right s ->
             case dir s of
               In -> throwError (show h ++ " is configured for input")
               Out ->
                 do states <- get
                    put (Map.insert h (s { value = v }) states)
                    say ["Write:", tshow h, tshow v]
                    next

pinExists :: (MonadMock m) => Pin -> m Bool
pinExists p = ask >>= return . (Set.member p)

validHandle :: (MonadMock m) => MockHandle -> m Bool
validHandle h = get >>= return . (Map.member h)

checkHandle :: (MonadError String m, MonadMock m) => MockHandle -> m Bool
checkHandle h =
  do valid <- validHandle h
     when (not valid) $
       throwError $ "Pin handle " ++ show h ++ " is invalid"
     return valid

say :: (MonadMock m) => [Text] -> m ()
say t = tell $ [T.intercalate " " t]

pinF :: (MonadMock m) => (MockState -> a) -> MockHandle -> m (Either String a)
pinF f h =
  do states <- get
     return $ fmap f (note (show h ++ " is not a valid pin handle") (Map.lookup h states))

mockState :: (MonadMock m) => MockHandle -> m (Either String MockState)
mockState = pinF id

pinValue :: (MonadMock m) => MockHandle -> m (Either String Value)
pinValue = pinF value

pinDirection :: (MonadMock m) => MockHandle -> m (Either String PinDirection)
pinDirection = pinF dir

-- | Run a GpioT program in a pure environment mimicking IO;
-- exceptions are manifested as 'Either' 'String' 'a'.
runMock :: AvailablePins -> MockT (ExceptT String Mock) a -> (Either String a, MockStateMap, [Text])
runMock pins action = runRWS (runExceptT $ runMockT action) pins Map.empty

-- | Evaluate a GpioT program in the 'Mock' monad and return the final
-- value and output, discarding the final state.
evalMock :: AvailablePins -> MockT (ExceptT String Mock) a -> (Either String a, [Text])
evalMock pins action =
  let (a, _, w) = runMock pins action
  in (a, w)

-- | Evaluate a GpioT program in the 'Mock' monad and return the final
-- state and output, discarding the final value.
execMock :: AvailablePins -> MockT (ExceptT String Mock) a -> (MockStateMap, [Text])
execMock pins action =
  let (_, s, w) = runMock pins action
  in (s, w)

tshow :: (Show a) => a -> Text
tshow = T.pack . show
