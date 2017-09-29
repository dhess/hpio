{-|

This program demonstrates how to use the 'SysfsGpioT' transformer with
a transformer stack.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Monad (forever, void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Options.Applicative
       (Parser, argument, auto, command, execParser, fullDesc, header,
        help, helper, hsubparser, info, long, metavar, option, progDesc,
        short, showDefault, value)
import System.GPIO.Linux.Sysfs (SysfsIOT, SysfsGpioT, runSysfsGpioT, runSysfsIOT, runSysfsGpioIO)
import System.GPIO.Monad
       (MonadGpio, Pin, PinInputMode(..), PinInterruptMode(..),
        PinOutputMode(..), PinValue(..), pins, pollPinTimeout,
        setPinInputMode, setPinInterruptMode, setPinOutputMode, togglePin,
        withPin)

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
       command "pollPin" (info pollPinCmd (progDesc "Drive INPIN using OUTPIN. (Make sure the pins are connected!")))

data Config =
  Config {pin :: Pin
         ,trigger :: PinInterruptMode
         ,wait :: Int}
  deriving ((Show))

-- | Our 'IO' transformer stack:
-- * A reader monad.
-- * The Linux @sysfs@ GPIO interpreter
-- * The (real) Linux @sysfs@ back-end.
-- * 'IO'
type SysfsGpioReaderIO a = ReaderT Config (SysfsGpioT (SysfsIOT IO)) a

-- | The interpreter for our IO transformer stack.
runSysfsGpioReaderIO :: SysfsGpioReaderIO a -> Config -> IO a
runSysfsGpioReaderIO act config = runSysfsIOT $ runSysfsGpioT $ runReaderT act config

run :: GlobalOptions -> IO ()
run (GlobalOptions SysfsIO (PollPin (PollPinOptions period mode to inputPin outputPin))) =
  void $
    concurrently
      (runSysfsGpioReaderIO pollInput (Config inputPin mode to))
      (runSysfsGpioReaderIO driveOutput (Config outputPin Disabled period))
-- The 'listPins' program takes no arguments, so we don't need our
-- custom 'IO' transformer stack here.
run (GlobalOptions SysfsIO ListPins) = runSysfsGpioIO listPins

output :: (MonadIO m) => String -> m ()
output = liftIO . putStrLn

listPins :: (MonadIO m, MonadGpio h m) => m ()
listPins =
  pins >>= \case
    [] -> output "No GPIO pins found on this system"
    ps -> for_ ps $ liftIO . print

pollInput :: (MonadMask m, MonadIO m, MonadGpio h m, MonadReader Config m) => m ()
pollInput =
  do p <- asks pin
     mode <- asks trigger
     timeout <- asks wait
     withPin p $ \h ->
       do setPinInputMode h InputDefault
          setPinInterruptMode h mode
          forever $
            do result <- pollPinTimeout h timeout
               case result of
                 Nothing -> output ("readPin timed out after " ++ show timeout ++ " microseconds")
                 Just v -> output ("Input: " ++ show v)

driveOutput :: (MonadMask m, MonadIO m, MonadGpio h m, MonadReader Config m) => m ()
driveOutput =
  do p <- asks pin
     delay <- asks wait
     withPin p $ \h ->
       do setPinOutputMode h OutputDefault Low
          forever $
            do liftIO $ threadDelay delay
               v <- togglePin h
               output ("Output: " ++ show v)

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info (helper <*> cmds)
           (fullDesc <>
            progDesc "Example hpio programs." <>
            header "hpio-reader-example - run hpio demonstrations.")
