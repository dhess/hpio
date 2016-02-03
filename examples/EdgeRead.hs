{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Options.Applicative
import System.GPIO.Free
import System.GPIO.Linux.Sysfs
import System.GPIO.Linux.SysfsIO (runSysfsIO)
import System.GPIO.Types
import System.Timeout (timeout)

data GlobalOptions =
  GlobalOptions {_delay :: Int
                ,_trigger :: PinReadTrigger
                ,_timeout :: Int
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
  option auto (long "timeout" <>
               short 'T' <>
               metavar "TIMEOUT" <>
               value (-1) <>
               help "Use a timeout for readPin (in ms)") <*>
  hsubparser
    (command "sysfs" (info sysfsCmd (progDesc "Use the Linux sysfs interpreter to drive INPIN using OUTPIN. (Make sure the pins are connected!")))

run :: GlobalOptions -> IO ()
run (GlobalOptions delay trigger to (Sysfs (SysfsOptions inputPin outputPin))) =
  do void $ forkIO (void $ runSysfsIO $ edgeRead inputPin trigger to)
     runSysfsIO $ driveOutput outputPin delay

output :: (MonadIO m) => String -> m ()
output = liftIO . putStrLn

-- This must be the 'SysfsT' monad rather than the more generic
-- 'GpioT' because we need to 'runSysfsIO' inside it, in order to use
-- 'timeout'.
edgeRead :: (MonadIO m) => Pin -> PinReadTrigger -> Int -> (SysfsT m) m ()
edgeRead p trigger to =
  withPin p $ \h ->
    do setPinDirection h In
       setPinReadTrigger h trigger
       forever $
         do result <- liftIO $ timeout to (runSysfsIO $ readPin h)
            case result of
              Nothing -> output ("readPin timed out after " ++ show to ++ " ms")
              Just v -> output ("Input: " ++ show v)

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
