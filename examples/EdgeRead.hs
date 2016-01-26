{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Error.Util (errLn)
import Control.Monad (forever, void, when)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Options.Applicative
import System.Exit (exitFailure)
import System.GPIO.Free
import System.GPIO.Linux.SysfsIO (runSysfsIOSafe)
import System.GPIO.Types

data GlobalOptions =
  GlobalOptions { cmd :: Command }

data Command
  = Sysfs SysfsOptions

data SysfsOptions =
  SysfsOptions {_outputPin :: Pin
               ,_inputPin :: Pin}

sysfsCmd :: Parser Command
sysfsCmd = Sysfs <$> sysfsOptions

sysfsOptions :: Parser SysfsOptions
sysfsOptions =
  SysfsOptions <$>
    argument auto (metavar "INPIN") <*>
    argument auto (metavar "OUTPIN")

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
  hsubparser
    (command "sysfs" (info sysfsCmd (progDesc "Use the Linux sysfs interpreter to drive INPIN using OUTPIN. (Make sure the pins are connected!")))

run :: GlobalOptions -> IO (Either String ())
run (GlobalOptions (Sysfs (SysfsOptions inputPin outputPin))) =
  do void $ forkIO (void $ runSysfsIOSafe $ edgeRead inputPin)
     runSysfsIOSafe $ driveOutput outputPin

output :: (MonadIO m) => String -> m ()
output = liftIO . putStrLn

edgeRead :: (MonadIO m, MonadError e m) => Pin -> GpioT e h m m ()
edgeRead p =
  withPin p $ \h ->
    do setPinDirection h In
       setPinReadTrigger h Level
       forever $
         do v <- readPin h
            output ("Input: " ++ show v)

driveOutput :: (MonadIO m, MonadError e m) => Pin -> GpioT e h m m ()
driveOutput p =
  withPin p $ \h ->
    do setPinDirection h Out
       forever $
         do liftIO $ threadDelay 100000
            v <- togglePinValue h
            output ("Output: " ++ show v)

main :: IO ()
main =
  do result <- execParser opts >>= run
     case result of
       Left e ->
         do errLn $ "Error: " ++ e
            exitFailure
       Right _ -> return ()
  where
    opts =
      info (helper <*> cmds)
           (fullDesc <>
            progDesc "Run the demo using the specified interpreter" <>
            header "edgeread - demonstrate gpio edge-triggered reads")
