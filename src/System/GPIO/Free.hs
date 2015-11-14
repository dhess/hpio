{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module System.GPIO.Free
       ( GpioF(..)
       , GpioM
       , GpioT
       , Pin(..)
       , PinDescriptor(..)
       , open
       , close
       ) where

import Control.Monad.Trans.Free (FreeT, MonadFree, liftF)
import Control.Monad.Free.TH (makeFreeCon)
import Data.Functor.Identity (Identity)
import GHC.Generics

data Pin = Pin Int deriving (Eq, Show, Generic)

data PinDescriptor = PinDescriptor Pin deriving (Show, Generic)

data GpioF next where
  Open :: Pin -> (Maybe PinDescriptor -> next) -> GpioF next
  Close :: PinDescriptor -> next -> GpioF next

instance Functor GpioF where
  fmap f (Open p g) = Open p (f . g)
  fmap f (Close d x) = Close d (f x)

type GpioT = FreeT GpioF

type GpioM = GpioT Identity

makeFreeCon 'Open
makeFreeCon 'Close
