{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Monad (forever, void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Options.Applicative
import System.GPIO.Linux.Sysfs (runSysfsGpioIO)
import System.GPIO.Monad

-- Only one for now.
data Interpreter =
  SysfsIO
  deriving (Eq,Show,Read)

data GlobalOptions =
  GlobalOptions {_interpreter :: !Interpreter
                ,_cmd :: !Command}

data Command
  = ListPins
  | PollPin PollPinOptions

listPinsCmd :: Parser Command
listPinsCmd = pure ListPins

data PollPinOptions =
  PollPinOptions {_period :: !Int
                 ,_trigger :: !PinInterruptMode
                 ,_timeout :: !Int
                 ,_outputPin :: !Pin
                 ,_inputPin :: !Pin}

pollPinCmd :: Parser Command
pollPinCmd = PollPin <$> pollPinOptions

oneSecond :: Int
oneSecond = 1 * 1000000

pollPinOptions :: Parser PollPinOptions
pollPinOptions =
  PollPinOptions <$>
    option auto (long "period" <>
                 short 'p' <>
                 metavar "INT" <>
                 value oneSecond <>
                 showDefault <>
                 help "Delay between output pin value toggles (in microseconds)") <*>
    option auto (long "trigger" <>
                 short 't' <>
                 metavar "Disabled|RisingEdge|FallingEdge|Level" <>
                 value Level <>
                 showDefault <>
                 help "Event on which to trigger the input pin") <*>
    option auto (long "timeout" <>
                 short 'T' <>
                 metavar "INT" <>
                 value (-1) <>
                 help "Poll timeout (in microseconds)") <*>
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
       command "pollPin" (info pollPinCmd (progDesc "Drive INPIN using OUTPIN and wait for interrupts. (Make sure the pins are connected!")))

run :: GlobalOptions -> IO ()
run (GlobalOptions SysfsIO (PollPin (PollPinOptions period trigger to inputPin outputPin))) =
  void $
    concurrently
      (void $ runSysfsGpioIO $ pollInput inputPin trigger to)
      (runSysfsGpioIO $ driveOutput outputPin period)
run (GlobalOptions SysfsIO ListPins) = runSysfsGpioIO listPins

output :: (MonadIO m) => String -> m ()
output = liftIO . putStrLn

listPins :: (Applicative m, MonadIO m, MonadGpio h m) => m ()
listPins =
  pins >>= \case
    [] -> output "No GPIO pins found on this system"
    ps -> for_ ps $ liftIO . print

pollInput :: (MonadMask m, MonadIO m, MonadGpio h m) => Pin -> PinInterruptMode -> Int -> m ()
pollInput p trigger to =
  withPin p $ \h ->
    do setPinInputMode h InputDefault
       setPinInterruptMode h trigger
       forever $
         do result <- pollPinTimeout h to
            case result of
              Nothing -> output ("readPin timed out after " ++ show to ++ " microseconds")
              Just v -> output ("Input: " ++ show v)

driveOutput :: (MonadMask m, MonadIO m, MonadGpio h m) => Pin -> Int -> m ()
driveOutput p delay =
  withPin p $ \h ->
    do setPinOutputMode h OutputDefault Low
       forever $
         do liftIO $ threadDelay delay
            v <- togglePin h
            output ("Output: " ++ show v)

main :: IO ()
main =execParser opts >>= run
  where
    opts =
      info (helper <*> cmds)
           (fullDesc <>
            progDesc "Example hpio programs." <>
            header "hpio-example - run hpio demonstrations.")
