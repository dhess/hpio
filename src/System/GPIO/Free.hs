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
       ) where

import Control.Monad.Trans.Free (FreeT, MonadFree, liftF)
import Control.Monad.Free.TH (makeFreeCon)
import Data.Functor.Identity (Identity)
import GHC.Generics

data Pin = Pin Int deriving (Eq, Ord, Show, Generic)

data PinDirection = In | Out deriving (Eq, Show, Generic)

data Value = Low | High deriving (Eq, Enum, Ord, Show, Generic)

data GpioF e h next where
  Pins :: ([Pin] -> next) -> GpioF e h next
  Open :: Pin -> (Either e h -> next) -> GpioF e h next
  Close :: h -> next -> GpioF e h next
  Direction :: h -> (Maybe PinDirection -> next) -> GpioF e h next
  SetDirection :: h -> PinDirection -> next -> GpioF e h next
  ReadPin :: h -> (Value -> next) -> GpioF e h next
  WritePin :: h -> Value -> next -> GpioF e h next

instance Functor (GpioF e h) where
  fmap f (Pins g) = Pins (f . g)
  fmap f (Open p g) = Open p (f . g)
  fmap f (Close h x) = Close h (f x)
  fmap f (Direction h g) = Direction h (f . g)
  fmap f (SetDirection h dir x) = SetDirection h dir (f x)
  fmap f (ReadPin h g) = ReadPin h (f . g)
  fmap f (WritePin h v x) = WritePin h v (f x)

type GpioT e h = FreeT (GpioF e h)

type GpioM h = (GpioT String h) Identity

makeFreeCon 'Pins
makeFreeCon 'Open
makeFreeCon 'Close
makeFreeCon 'Direction
makeFreeCon 'SetDirection
makeFreeCon 'ReadPin
makeFreeCon 'WritePin
