{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module System.GPIO.Free
       ( GpioF(..)
       , GpioM
       , GpioT
       , Direction(..)
       , Pin(..)
       , PinDescriptor(..)
       , Value(..)
       , open
       , close
       , hasDirection
       , getDirection
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

data Direction = In | Out deriving (Eq, Show, Generic)

data Value = Low | High deriving (Eq, Enum, Ord, Show, Generic)

data GpioF next where
  Open :: Pin -> (Either String PinDescriptor -> next) -> GpioF next
  Close :: PinDescriptor -> next -> GpioF next
  HasDirection :: PinDescriptor -> (Bool -> next) -> GpioF next
  GetDirection :: PinDescriptor -> (Direction -> next) -> GpioF next
  SetDirection :: PinDescriptor -> Direction -> next -> GpioF next
  ReadPin :: PinDescriptor -> (Value -> next) -> GpioF next
  WritePin :: PinDescriptor -> Value -> next -> GpioF next

instance Functor GpioF where
  fmap f (Open p g) = Open p (f . g)
  fmap f (Close pd x) = Close pd (f x)
  fmap f (HasDirection pd g) = HasDirection pd (f . g)
  fmap f (GetDirection pd g) = GetDirection pd (f . g)
  fmap f (SetDirection pd d x) = SetDirection pd d (f x)
  fmap f (ReadPin pd g) = ReadPin pd (f . g)
  fmap f (WritePin pd v x) = WritePin pd v (f x)

type GpioT = FreeT GpioF

type GpioM = GpioT Identity

makeFreeCon 'Open
makeFreeCon 'Close
makeFreeCon 'HasDirection
makeFreeCon 'GetDirection
makeFreeCon 'SetDirection
makeFreeCon 'ReadPin
makeFreeCon 'WritePin
