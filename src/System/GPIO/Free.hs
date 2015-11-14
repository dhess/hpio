{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module System.GPIO.Free
       ( GpioF(..)
       , GpioM
       , Pin(..)
       , allocPin
       , deallocPin
       ) where

import Control.Monad.Free (Free, MonadFree, liftF)
import Control.Monad.Free.TH (makeFreeCon)
import GHC.Generics

data Pin = Pin Int deriving (Eq, Show, Generic)

data GpioF next where
  AllocPin :: Pin -> next -> GpioF next
  DeallocPin :: Pin -> next -> GpioF next

instance Functor GpioF where
  fmap f (AllocPin p x) = AllocPin p (f x)
  fmap f (DeallocPin p x) = DeallocPin p (f x)

type GpioM = Free GpioF

makeFreeCon 'AllocPin
makeFreeCon 'DeallocPin
