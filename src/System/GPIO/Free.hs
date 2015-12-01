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
       , PinDescriptor(..)
       , PinDirection(..)
       , Value(..)
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

data PinDescriptor = PinDescriptor Pin deriving (Eq, Ord, Show, Generic)

data PinDirection = In | Out deriving (Eq, Show, Generic)

data Value = Low | High deriving (Eq, Enum, Ord, Show, Generic)

data GpioF e next where
  Open :: Pin -> (Either e PinDescriptor -> next) -> GpioF e next
  Close :: PinDescriptor -> next -> GpioF e next
  Direction :: PinDescriptor -> (Maybe PinDirection -> next) -> GpioF e next
  SetDirection :: PinDescriptor -> PinDirection -> next -> GpioF e next
  ReadPin :: PinDescriptor -> (Value -> next) -> GpioF e next
  WritePin :: PinDescriptor -> Value -> next -> GpioF e next

instance Functor (GpioF e) where
  fmap f (Open p g) = Open p (f . g)
  fmap f (Close pd x) = Close pd (f x)
  fmap f (Direction pd g) = Direction pd (f . g)
  fmap f (SetDirection pd d x) = SetDirection pd d (f x)
  fmap f (ReadPin pd g) = ReadPin pd (f . g)
  fmap f (WritePin pd v x) = WritePin pd v (f x)

type GpioT e = FreeT (GpioF e)

type GpioM = (GpioT String) Identity

makeFreeCon 'Open
makeFreeCon 'Close
makeFreeCon 'Direction
makeFreeCon 'SetDirection
makeFreeCon 'ReadPin
makeFreeCon 'WritePin
