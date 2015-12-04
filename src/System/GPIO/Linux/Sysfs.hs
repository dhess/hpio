{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs
       ( PinDescriptor(..)
       , SysfsF
       , SysfsT
       , runSysfsT
       ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Free (iterT)
import System.FilePath
import System.GPIO.Free (GpioF(..), GpioT, PinDirection(..), Pin(..), Value(..))
import System.IO

sysfsPath :: FilePath
sysfsPath = "/sys/class/gpio"

exportFile :: FilePath
exportFile = sysfsPath </> "export"

unexportFile :: FilePath
unexportFile = sysfsPath </> "unexport"

pinPath :: Pin -> FilePath
pinPath (Pin n) = sysfsPath </> ("gpio" ++ show n)

directionFile :: Pin -> FilePath
directionFile p = pinPath p </> "direction"

valueFile :: Pin -> FilePath
valueFile p = pinPath p </> "value"

newtype PinDescriptor = PinDescriptor { pin :: Pin } deriving (Show, Eq, Ord)

type SysfsT = GpioT String PinDescriptor

type SysfsF = GpioF String PinDescriptor

runSysfsT :: (MonadIO m) => SysfsT m a -> m a
runSysfsT = iterT run
  where
    run :: (MonadIO m) => SysfsF (m a) -> m a

    run (Open (Pin n) next) =
      do undefined

    run (Close d next) =
      do undefined

    run (Direction d next) =
      do undefined

    run (SetDirection d v next) =
      do undefined

    run (ReadPin d next) =
      do undefined

    run (WritePin d v next) =
      do undefined
