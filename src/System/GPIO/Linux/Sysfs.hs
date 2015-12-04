{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs
       ( PinDescriptor(..)
       , SysfsF
       , SysfsT
       , runSysfsT
       ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Free (iterT)
import Data.Char (toLower)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath
import System.GPIO.Free (GpioF(..), GpioT, PinDirection(..), Pin(..), Value(..))
import qualified System.IO as IO (writeFile)
import qualified System.IO.Strict as IOS (readFile)

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

    -- Export the pin. Note that it may already be exported, which we
    -- treat as success.
    run (Open p@(Pin n) next) =
      do hasSysfs <- liftIO $ doesDirectoryExist sysfsPath
         case hasSysfs of
           False -> next (Left "The system does not support sysfs GPIO")
           True ->
             do alreadyExported <- liftIO $ doesFileExist (pinPath p)
                case alreadyExported of
                  True -> next (Right $ PinDescriptor p)
                  False ->
                    do void $ writeFile exportFile (show n)
                       next (Right $ PinDescriptor p)

    run (Close d next) =
      do let (Pin n) = pin d
         void $ writeFile unexportFile (show n)
         next

    run (Direction d next) =
      do let p = pin d
         hasDir <- hasDirection p
         case hasDir of
           False -> next Nothing
           True ->
             do dir <- readFile (directionFile p)
                case dir of
                  "in"  -> next $ Just In
                  "out" -> next $ Just Out
                  _     -> next Nothing -- XXX: should be an exception

    run (SetDirection d dir next) =
      do let p = pin d
         void $ writeFile (directionFile p) (lowercase $ show dir)
         next

    run (ReadPin d next) =
      do let p = pin d
         value <- readFile (valueFile p)
         case value of
           "0" -> next Low
           "1" -> next High
           _   -> next High -- XXX: should be an exception

    run (WritePin d v next) =
      do let p = pin d
         void $ writeFile (valueFile p) (toSysfsValue v)
         next

toSysfsValue :: Value -> String
toSysfsValue Low = "0"
toSysfsValue High = "1"

lowercase :: String -> String
lowercase = fmap toLower

hasDirection :: (MonadIO m) => Pin -> m Bool
hasDirection p = liftIO $ doesFileExist (directionFile p)

writeFile :: (MonadIO m) => FilePath -> String -> m ()
writeFile f s = liftIO $ IO.writeFile f s

readFile :: (MonadIO m) => FilePath -> m String
readFile f = liftIO $ IOS.readFile f
