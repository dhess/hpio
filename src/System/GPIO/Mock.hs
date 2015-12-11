-- | An interpreter for testing 'GpioF' programs. It provides a mock
-- IO environment that works on any system, even those without real
-- GPIO capabilities.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.GPIO.Mock
       ( -- * A mock GPIO interpreter
         MockF
       , MockT
       , runMockT
       , runMock
       , execMock
       , evalMock
         -- * Mock types
       , MockPins
       , MockHandle(..)
       , Mock
       , MockState(..)
       , MockStateMap
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
import System.GPIO.Free (GpioF(..), GpioT, Pin(..), PinDirection(..), Value(..), openPin, closePin)

-- | The set of all available pins in the mock environment.
type MockPins = Set Pin

-- | Keep track of the state of opened mock pins.
data MockState = MockState { dir :: !PinDirection, value :: !Value } deriving (Show, Eq)

-- | A handle for opened mock pins.
data MockHandle = MockHandle Pin deriving (Show, Eq, Ord)

-- | Maps mock handles to their pin state.
type MockStateMap = Map MockHandle MockState

-- | Initial state of a newly-opened mock pin.
initialState :: MockState
initialState = MockState In Low

-- | The interface for all suitable monads in the mock system.
type MonadMock = MonadRWS MockPins [Text] MockStateMap

-- | The simplest possible mock monad.
type Mock = RWS MockPins [Text] MockStateMap

-- | A transformer for adding the mock interpreter to an existing
-- monad. Note that it represents errors as 'String'.
type MockT m = GpioT String MockHandle m

-- | The mock eDSL.
type MockF m = GpioF String MockHandle m

-- | Run a 'GpioF' computation in the 'MockT' transformer, and return
-- the result.
--
-- If an error occurs in the computation, it is thrown as an exception
-- whose error information type is 'String'.
runMockT :: (MonadError String m, MonadMock m) => (MockT m) m a -> m a
runMockT = iterT run
  where
    run :: (MonadError String m, MonadMock m) => (MockF m) (m a) -> m a

    run (Pins next) = ask >>= next . Set.toList

    run (OpenPin p next) =
      do valid <- pinExists p
         case valid of
           False -> next (Left $ "Open failed: " ++ show p ++ " does not exist")
           True ->
             do say ["Open", tshow p]
                states <- get
                let d = MockHandle p
                put (Map.insert d initialState states)
                next (Right d)

    run (ClosePin h next) =
      do valid <- validHandle h
         case valid of
           False -> next -- double-close is OK
           True ->
             do states <- get
                put (Map.delete h states)
                say ["Close", tshow h]
                next

    run (GetPinDirection h next) =
      do void $ checkHandle h
         eitherDirection <- pinDirection h
         case eitherDirection of
           Left e -> throwError e
           Right dir' -> next (Just dir')

    run (SetPinDirection h v next) =
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

    run (WithPin p block next) =
      do openResult <- runMockT $ openPin p
         case openResult of
           Left e1 -> throwError e1
           Right handle ->
             catchError
               (do a <- runMockT $ block handle
                   runMockT $ closePin handle
                   next a)
               (\e ->
                 do runMockT $ closePin handle
                    throwError e)

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

-- | Run a 'GpioF' program in an environment mimicking IO and return
-- the result as 'Right' 'a'; the state of the mock IO environment at
-- the end of the computation; and a log of the GPIO side effects that
-- were performed during the computation. If an exception occurs in
-- the 'GpioF' program, the value part of the returned tuple is
-- returned as 'Left' 'String'.
runMock :: MockPins -> MockT (ExceptT String Mock) (ExceptT String Mock) a -> (Either String a, MockStateMap, [Text])
runMock pins action = runRWS (runExceptT $ runMockT action) pins Map.empty

-- | Evaluate a 'GpioF' program in the 'Mock' monad and return the final
-- value and log, discarding the final state.
evalMock :: MockPins -> MockT (ExceptT String Mock) (ExceptT String Mock) a -> (Either String a, [Text])
evalMock pins action =
  let (a, _, w) = runMock pins action
  in (a, w)

-- | Evaluate a 'GpioF' program in the 'Mock' monad and return the final
-- state and log, discarding the final value.
execMock :: MockPins -> MockT (ExceptT String Mock) (ExceptT String Mock) a -> (MockStateMap, [Text])
execMock pins action =
  let (_, s, w) = runMock pins action
  in (s, w)

tshow :: (Show a) => a -> Text
tshow = T.pack . show
