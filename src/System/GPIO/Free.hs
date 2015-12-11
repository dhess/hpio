{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module System.GPIO.Free
       ( GpioF(..)
       , GpioM
       , GpioT
       , Pin(..)
       , PinDirection(..)
       , Value(..)
       , pins
       , open
       , close
       , direction
       , setDirection
       , readPin
       , writePin
       , withPin
       ) where

import Control.Monad.Trans.Free (FreeT, MonadFree, liftF)
import Control.Monad.Free.TH (makeFreeCon)
import Data.Functor.Identity (Identity)
import GHC.Generics

data Pin = Pin Int deriving (Eq, Ord, Show, Generic)

data PinDirection = In | Out deriving (Eq, Show, Generic)

data Value = Low | High deriving (Eq, Enum, Ord, Show, Generic)

data GpioF e h m next where
  Pins :: ([Pin] -> next) -> GpioF e h m next
  Open :: Pin -> (Either e h -> next) -> GpioF e h m next
  Close :: h -> next -> GpioF e h m next
  Direction :: h -> (Maybe PinDirection -> next) -> GpioF e h m next
  SetDirection :: h -> PinDirection -> next -> GpioF e h m next
  ReadPin :: h -> (Value -> next) -> GpioF e h m next
  WritePin :: h -> Value -> next -> GpioF e h m next
  WithPin :: Pin -> (h -> GpioT e h m m a) -> (a -> next) -> GpioF e h m next

instance Functor (GpioF e h m) where
  fmap f (Pins g) = Pins (f . g)
  fmap f (Open p g) = Open p (f . g)
  fmap f (Close h x) = Close h (f x)
  fmap f (Direction h g) = Direction h (f . g)
  fmap f (SetDirection h dir x) = SetDirection h dir (f x)
  fmap f (ReadPin h g) = ReadPin h (f . g)
  fmap f (WritePin h v x) = WritePin h v (f x)
  fmap f (WithPin p block g) = WithPin p block (f . g)

type GpioT e h m = FreeT (GpioF e h m)

type GpioM h = (GpioT String h Identity) Identity

makeFreeCon 'Pins
makeFreeCon 'Open
makeFreeCon 'Close
makeFreeCon 'Direction
makeFreeCon 'SetDirection
makeFreeCon 'ReadPin
makeFreeCon 'WritePin
makeFreeCon 'WithPin
