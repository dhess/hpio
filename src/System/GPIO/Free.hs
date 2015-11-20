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
       , State(..)
       , open
       , close
       , hasDirection
       , getDirection
       , setDirection
       , read
       , write
       ) where

import Prelude hiding (read)
import Control.Monad.Trans.Free (FreeT, MonadFree, liftF)
import Control.Monad.Free.TH (makeFreeCon)
import Data.Functor.Identity (Identity)
import GHC.Generics

data Pin = Pin Int deriving (Eq, Ord, Show, Generic)

data PinDescriptor = PinDescriptor Pin deriving (Eq, Ord, Show, Generic)

data Direction = In | Out deriving (Eq, Show, Generic)

data State = Low | High deriving (Eq, Enum, Ord, Show, Generic)

data GpioF next where
  Open :: Pin -> (Either String PinDescriptor -> next) -> GpioF next
  Close :: PinDescriptor -> next -> GpioF next
  HasDirection :: PinDescriptor -> (Bool -> next) -> GpioF next
  GetDirection :: PinDescriptor -> (Either String Direction -> next) -> GpioF next
  SetDirection :: PinDescriptor -> v -> (Either String Direction -> next) -> GpioF next
  Read :: PinDescriptor -> (State -> next) -> GpioF next
  Write :: PinDescriptor -> State -> (Either String State -> next) -> GpioF next

instance Functor GpioF where
  fmap f (Open p g) = Open p (f . g)
  fmap f (Close pd x) = Close pd (f x)
  fmap f (HasDirection pd g) = HasDirection pd (f . g)
  fmap f (GetDirection pd g) = GetDirection pd (f . g)
  fmap f (SetDirection pd v g) = SetDirection pd v (f . g)
  fmap f (Read pd g) = Read pd (f . g)
  fmap f (Write pd s g) = Write pd s (f . g)

type GpioT = FreeT GpioF

type GpioM = GpioT Identity

makeFreeCon 'Open
makeFreeCon 'Close
makeFreeCon 'HasDirection
makeFreeCon 'GetDirection
makeFreeCon 'SetDirection
makeFreeCon 'Read
makeFreeCon 'Write
