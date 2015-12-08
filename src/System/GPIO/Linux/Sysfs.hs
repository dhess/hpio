{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs
       ( PinDescriptor(..)
       , SysfsF
       , SysfsT
       , runSysfsT
       , sysfsPath
       , exportFileName
       , unexportFileName
       , pinDirName
       , pinDirectionFileName
       , pinValueFileName
       ) where

import Prelude hiding (readFile, writeFile)
import Control.Error.Util (hushT)
import Control.Error.Script (scriptIO)
import Control.Monad (filterM, void)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Free (iterT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Char (toLower)
import Data.List (isPrefixOf, sort)
import Data.Maybe (catMaybes)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath
import System.GPIO.Free (GpioF(..), GpioT, PinDirection(..), Pin(..), Value(..))
import qualified System.IO as IO (writeFile)
import qualified System.IO.Strict as IOS (readFile)

sysfsPath :: FilePath
sysfsPath = "/sys/class/gpio"

exportFileName :: FilePath
exportFileName = sysfsPath </> "export"

unexportFileName :: FilePath
unexportFileName = sysfsPath </> "unexport"

pinDirName :: Pin -> FilePath
pinDirName (Pin n) = sysfsPath </> ("gpio" ++ show n)

pinDirectionFileName :: Pin -> FilePath
pinDirectionFileName p = pinDirName p </> "direction"

pinValueFileName :: Pin -> FilePath
pinValueFileName p = pinDirName p </> "value"

newtype PinDescriptor = PinDescriptor { pin :: Pin } deriving (Show, Eq, Ord)

type SysfsT = GpioT String PinDescriptor

type SysfsF = GpioF String PinDescriptor

runSysfsT :: (MonadError String m, MonadIO m) => SysfsT m a -> m a
runSysfsT = iterT run
  where
    run :: (MonadError String m, MonadIO m) => SysfsF (m a) -> m a

    run (Pins next) =
      do hasSysfs <- liftIO $ doesDirectoryExist sysfsPath
         case hasSysfs of
           False -> next []
           True -> allPins >>= next

    -- Export the pin. Note that it may already be exported, which we
    -- treat as success.
    run (Open p@(Pin n) next) =
      do hasSysfs <- liftIO $ doesDirectoryExist sysfsPath
         case hasSysfs of
           False -> next (Left "sysfs GPIO is not present")
           True ->
             do exported <- liftIO $ doesDirectoryExist (pinDirName p)
                case exported of
                  True -> next (Right $ PinDescriptor p)
                  False ->
                    do void $ writeFile exportFileName (show n)
                       next (Right $ PinDescriptor p)

    run (Close d next) =
      do let (Pin n) = pin d
         void $ writeFile unexportFileName (show n)
         next

    run (Direction d next) =
      do let p = pin d
         hasDirection <- liftIO $ doesFileExist (pinDirectionFileName p)
         case hasDirection of
           False -> next Nothing
           True ->
             do dir <- readFile (pinDirectionFileName p)
                case dir of
                  "in\n"  -> next $ Just In
                  "out\n" -> next $ Just Out
                  _     -> next Nothing -- XXX: should be an exception

    run (SetDirection d dir next) =
      do let p = pin d
         void $ writeFile (pinDirectionFileName p) (lowercase $ show dir)
         next

    run (ReadPin d next) =
      do let p = pin d
         value <- readFile (pinValueFileName p)
         case value of
           "0\n" -> next Low
           "1\n" -> next High
           _   -> next High -- XXX: should be an exception

    run (WritePin d v next) =
      do let p = pin d
         void $ writeFile (pinValueFileName p) (toSysfsValue v)
         next

toSysfsValue :: Value -> String
toSysfsValue Low = "0"
toSysfsValue High = "1"

lowercase :: String -> String
lowercase = fmap toLower

writeFile :: (MonadIO m) => FilePath -> String -> m ()
writeFile f s = liftIO $ IO.writeFile f s

readFile :: (MonadIO m) => FilePath -> m String
readFile f = liftIO $ IOS.readFile f

readFromFile :: (MonadIO m, Read a) => FilePath -> m a
readFromFile f = liftIO (IOS.readFile f >>= readIO)

maybeIO :: (MonadIO m) => IO a -> MaybeT m a
maybeIO = hushT . scriptIO

chipBaseGpio :: (MonadIO m) => FilePath -> m Int
chipBaseGpio chipDir = readFromFile (chipDir </> "base")

chipNGpio :: (MonadIO m) => FilePath -> m Int
chipNGpio chipDir = readFromFile (chipDir </> "ngpio")

allPins :: (MonadIO m) => m [Pin]
allPins =
  do sysfsEntries <- liftIO $ getDirectoryContents sysfsPath
     let sysfsContents = fmap (sysfsPath </>) sysfsEntries
     sysfsDirectories <- filterM (liftIO . doesDirectoryExist) sysfsContents
     let chipDirs = filter (\f -> isPrefixOf "gpiochip" $ takeFileName f) sysfsDirectories
     maybePins <- mapM (runMaybeT . pinRange) chipDirs
     return $ sort $ concat $ catMaybes maybePins

pinRange :: (MonadIO m) => FilePath -> MaybeT m [Pin]
pinRange chipDir =
  do base <- maybeIO $ chipBaseGpio chipDir
     ngpio <- maybeIO $ chipNGpio chipDir
     case (base >= 0 && ngpio > 0) of
       False -> return []
       True -> return $ fmap Pin [base .. (base + ngpio - 1)]
