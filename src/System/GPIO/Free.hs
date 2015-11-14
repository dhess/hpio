{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module System.GPIO.Free
       ( GpioF(..)
       , GpioM
       , GpioT
       , Pin(..)
       , allocPin
       , deallocPin
       ) where

import Control.Monad.Trans.Free (FreeT, MonadFree, liftF)
import Control.Monad.Free.TH (makeFreeCon)
import Data.Functor.Identity (Identity)
import GHC.Generics

data Pin = Pin Int deriving (Eq, Show, Generic)

data GpioF next where
  AllocPin :: Pin -> next -> GpioF next
  DeallocPin :: Pin -> next -> GpioF next

instance Functor GpioF where
  fmap f (AllocPin p x) = AllocPin p (f x)
  fmap f (DeallocPin p x) = DeallocPin p (f x)

type GpioT = FreeT GpioF

type GpioM = GpioT Identity

makeFreeCon 'AllocPin
makeFreeCon 'DeallocPin
