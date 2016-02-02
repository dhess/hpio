{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Options.Applicative
import System.GPIO.Free
import System.GPIO.Linux.SysfsIO (runSysfsIO)
import System.GPIO.Types

data GlobalOptions =
  GlobalOptions {_delay :: Int
                ,_trigger :: PinReadTrigger
                ,_cmd :: Command}

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
  option auto (long "delay" <>
               short 'd' <>
               metavar "DELAY" <>
               value 1 <>
               showDefault <>
               help "Delay between output toggles (in seconds)") <*>
  option auto (long "trigger" <>
               short 't' <>
               metavar "TRIGGER" <>
               value Level <>
               showDefault <>
               help "Event on which to trigger (None, RisingEdge, FallingEdge, or Level)") <*>
  hsubparser
    (command "sysfs" (info sysfsCmd (progDesc "Use the Linux sysfs interpreter to drive INPIN using OUTPIN. (Make sure the pins are connected!")))

run :: GlobalOptions -> IO ()
run (GlobalOptions delay trigger (Sysfs (SysfsOptions inputPin outputPin))) =
  do void $ forkIO (void $ runSysfsIO $ edgeRead inputPin trigger)
     runSysfsIO $ driveOutput outputPin delay

output :: (MonadIO m) => String -> m ()
output = liftIO . putStrLn

edgeRead :: (MonadIO m) => Pin -> PinReadTrigger -> GpioT h m m ()
edgeRead p trigger =
  withPin p $ \h ->
    do setPinDirection h In
       setPinReadTrigger h trigger
       forever $
         do v <- readPin h
            output ("Input: " ++ show v)

driveOutput :: (MonadIO m) => Pin -> Int -> GpioT h m m ()
driveOutput p delay =
  withPin p $ \h ->
    do setPinDirection h Out
       forever $
         do liftIO $ threadDelay (delay * 1000000)
            v <- togglePinValue h
            output ("Output: " ++ show v)

main :: IO ()
main =execParser opts >>= run
  where
    opts =
      info (helper <*> cmds)
           (fullDesc <>
            progDesc "Run the demo using the specified interpreter" <>
            header "edgeread - demonstrate gpio edge-triggered reads")
