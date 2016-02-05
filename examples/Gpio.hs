{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Options.Applicative
import System.GPIO.Free
import System.GPIO.Linux.Sysfs
import System.GPIO.Linux.SysfsIO (runSysfsIO)
import System.GPIO.Types
import System.Timeout (timeout)

-- Only one for now.
data Interpreter =
  SysfsIO
  deriving (Eq,Show,Read)

data GlobalOptions =
  GlobalOptions {_interpreter :: Interpreter
                ,_cmd :: Command}

data Command
  = ListPins
  | ReadTrigger ReadTriggerOptions

listPinsCmd :: Parser Command
listPinsCmd = pure ListPins

data ReadTriggerOptions =
  ReadTriggerOptions {_period :: Int
                     ,_trigger :: PinReadTrigger
                     ,_timeout :: Int
                     ,_outputPin :: Pin
                     ,_inputPin :: Pin}

readTriggerCmd :: Parser Command
readTriggerCmd = ReadTrigger <$> readTriggerOptions

oneSecond :: Int
oneSecond = 1 * 1000000

readTriggerOptions :: Parser ReadTriggerOptions
readTriggerOptions =
  ReadTriggerOptions <$>
    option auto (long "period" <>
                 short 'p' <>
                 metavar "INT" <>
                 value oneSecond <>
                 showDefault <>
                 help "Delay between output pin value toggles (in ms)") <*>
    option auto (long "trigger" <>
                 short 't' <>
                 metavar "None|RisingEdge|FallingEdge|Level" <>
                 value Level <>
                 showDefault <>
                 help "Event on which to trigger the input pin") <*>
    option auto (long "timeout" <>
                 short 'T' <>
                 metavar "INT" <>
                 value (-1) <>
                 help "Use a timeout for readPin (in ms)") <*>
    argument auto (metavar "INPIN")  <*>
    argument auto (metavar "OUTPIN")

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
    option auto (long "interpreter" <>
                 short 'i' <>
                 metavar "SysfsIO" <>
                 value SysfsIO <>
                 showDefault <>
                 help "Choose the GPIO interpreter (system) to use") <*>
    hsubparser
      (command "listPins" (info listPinsCmd (progDesc "List the GPIO pins available on the system")) <>
       command "readTrigger" (info readTriggerCmd (progDesc "Use the Linux sysfs interpreter to drive INPIN using OUTPIN. (Make sure the pins are connected!")))

run :: GlobalOptions -> IO ()
run (GlobalOptions SysfsIO (ReadTrigger (ReadTriggerOptions period trigger to inputPin outputPin))) =
  do void $ forkIO (void $ runSysfsIO $ edgeRead inputPin trigger to)
     runSysfsIO $ driveOutput outputPin period
run (GlobalOptions SysfsIO ListPins) = runSysfsIO listPins

output :: (MonadIO m) => String -> m ()
output = liftIO . putStrLn

listPins :: (MonadIO m) => GpioT h m m ()
listPins =
  pins >>= \case
    [] -> output "No GPIO pins found on this system"
    ps -> for_ ps $ liftIO . print

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
            progDesc "Example gpio programs." <>
            header "gpio-example - run gpio demonstrations.")
