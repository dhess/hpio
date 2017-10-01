{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Protolude uses <> and options from Semigroups, but
-- optparse-applicative hasn't caught up yet.
import Protolude hiding ((<>))
import Control.Monad.Catch (MonadMask)
import Data.Monoid ((<>))
import Data.Text (unwords)
import Options.Applicative
       (Parser, argument, auto, command, execParser, fullDesc, header,
        help, helper, hsubparser, info, long, metavar, progDesc, short,
        showDefault, value)
import qualified Options.Applicative as Options (option)
import System.GPIO.Linux.Sysfs (runSysfsGpioIO)
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
    Options.option auto (long "period" <>
                         short 'p' <>
                         metavar "INT" <>
                         value oneSecond <>
                         showDefault <>
                         help "Delay between output pin value toggles (in microseconds)") <*>
    Options.option auto (long "trigger" <>
                         short 't' <>
                         metavar "Disabled|RisingEdge|FallingEdge|Level" <>
                         value Level <>
                         showDefault <>
                         help "Event on which to trigger the input pin") <*>
    Options.option auto (long "timeout" <>
                         short 'T' <>
                         metavar "INT" <>
                         value (-1) <>
                         help "Poll timeout (in microseconds)") <*>
    argument auto (metavar "INPIN")  <*>
    argument auto (metavar "OUTPIN")

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
    Options.option auto (long "interpreter" <>
                         short 'i' <>
                         metavar "SysfsIO" <>
                         value SysfsIO <>
                         showDefault <>
                         help "Choose the GPIO interpreter (system) to use") <*>
    hsubparser
      (command "listPins" (info listPinsCmd (progDesc "List the GPIO pins available on the system")) <>
       command "pollPin" (info pollPinCmd (progDesc "Drive INPIN using OUTPIN and wait for interrupts. (Make sure the pins are connected!")))

run :: GlobalOptions -> IO ()
run (GlobalOptions SysfsIO (PollPin (PollPinOptions period trigger timeout inputPin outputPin))) =
  void $
    concurrently
      (void $ runSysfsGpioIO $ pollInput inputPin trigger timeout)
      (runSysfsGpioIO $ driveOutput outputPin period)
run (GlobalOptions SysfsIO ListPins) = runSysfsGpioIO listPins

-- | Define a constraint that can work with multiple 'MonadGpio'
-- interpreters.
type GpioM h m = (MonadMask m, MonadIO m, MonadGpio h m)

listPins :: (GpioM h m) => m ()
listPins =
  pins >>= \case
    [] -> putText "No GPIO pins found on this system"
    ps -> for_ ps $ liftIO . print

pollInput :: (GpioM h m) => Pin -> PinInterruptMode -> Int -> m ()
pollInput p trigger timeout =
  withPin p $ \h ->
    do setPinInputMode h InputDefault
       setPinInterruptMode h trigger
       forever $
         do result <- pollPinTimeout h timeout
            case result of
              Nothing -> putText $ unwords ["readPin timed out after", show timeout, "microseconds"]
              Just v -> putText $ unwords ["Input:", show v]

driveOutput :: (GpioM h m) => Pin -> Int -> m ()
driveOutput p delay =
  withPin p $ \h ->
    do setPinOutputMode h OutputDefault Low
       forever $
         do liftIO $ threadDelay delay
            v <- togglePin h
            putText $ unwords ["Output:", show v]

main :: IO ()
main =execParser opts >>= run
  where
    opts =
      info (helper <*> cmds)
           (fullDesc <>
            progDesc "Example hpio programs." <>
            header "hpio-example - run hpio demonstrations.")
