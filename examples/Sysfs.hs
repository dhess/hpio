{-|

This program demonstrates how to use the native Linux @sysfs@ GPIO
implementation directly, without using the
'System.GPIO.Monad.MonadGpio' monad class.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket_)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Text (unwords)
import Options.Applicative
       (Parser, argument, auto, command, execParser, fullDesc, header,
        help, helper, hsubparser, info, long, metavar, option, progDesc,
        short, showDefault, value)
import System.GPIO.Linux.Sysfs.IO (SysfsIOT(..))
import System.GPIO.Linux.Sysfs.Monad
       (availablePins, exportPin, pollPinValueTimeout, readPinValue,
        unexportPin, writePinDirection, writePinEdge, writePinValue)
import System.GPIO.Linux.Sysfs.Types (SysfsEdge(..))
import System.GPIO.Types (Pin, PinDirection(..), invertValue)

data GlobalOptions =
  GlobalOptions {_cmd :: !Command}

data Command
  = ListPins
  | ReadEdge ReadEdgeOptions

listPinsCmd :: Parser Command
listPinsCmd = pure ListPins

data ReadEdgeOptions =
  ReadEdgeOptions {_period :: !Int
                  ,_edge :: !SysfsEdge
                  ,_timeout :: !Int
                  ,_outputPin :: !Pin
                  ,_inputPin :: !Pin}

readEdgeCmd :: Parser Command
readEdgeCmd = ReadEdge <$> readEdgeOptions

oneSecond :: Int
oneSecond = 1 * 1000000

readEdgeOptions :: Parser ReadEdgeOptions
readEdgeOptions =
  ReadEdgeOptions <$>
    option auto (long "period" <>
                 short 'p' <>
                 metavar "INT" <>
                 value oneSecond <>
                 showDefault <>
                 help "Delay between output pin value toggles (in microseconds)") <*>
    option auto (long "edge" <>
                 short 'e' <>
                 metavar "None|Rising|Falling|Both" <>
                 value Both <>
                 showDefault <>
                 help "Edge on which to trigger the input pin") <*>
    option auto (long "timeout" <>
                 short 'T' <>
                 metavar "INT" <>
                 value (-1) <>
                 help "Use a timeout for readPin (in microseconds)") <*>
    argument auto (metavar "INPIN")  <*>
    argument auto (metavar "OUTPIN")

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
    hsubparser
      (command "listPins" (info listPinsCmd (progDesc "List the GPIO pins available on the system")) <>
       command "readEdge" (info readEdgeCmd (progDesc "Drive INPIN using OUTPIN. (Make sure the pins are connected!")))

type NativeSysfs a = SysfsIOT IO a

runNativeSysfs :: NativeSysfs a -> IO a
runNativeSysfs = runSysfsIOT

run :: GlobalOptions -> IO ()
run (GlobalOptions (ReadEdge (ReadEdgeOptions period edge timeout inputPin outputPin))) =
  void $
    concurrently
      (runNativeSysfs $ edgeRead inputPin edge timeout)
      (runNativeSysfs $ driveOutput outputPin period)
run (GlobalOptions ListPins) = runNativeSysfs listPins

withPin :: Pin -> NativeSysfs a -> NativeSysfs a
withPin p block = liftIO $ bracket_ (runNativeSysfs $ exportPin p) (runNativeSysfs $ unexportPin p) (runNativeSysfs block)

listPins :: NativeSysfs ()
listPins =
  availablePins >>= \case
    [] -> putText "No GPIO pins found on this system"
    ps -> for_ ps $ putText . show

edgeRead :: Pin -> SysfsEdge -> Int -> NativeSysfs ()
edgeRead p edge timeout =
  withPin p $
    do writePinDirection p In
       writePinEdge p edge
       forever $
         do result <- pollPinValueTimeout p timeout
            case result of
              Nothing -> putText $ unwords ["readPin timed out after", show timeout, "microseconds"]
              Just v -> putText $ unwords ["Input:", show v]

driveOutput :: Pin -> Int -> NativeSysfs ()
driveOutput p delay =
  withPin p $
    do writePinDirection p Out
       forever $
         do liftIO $ threadDelay delay
            v <- readPinValue p
            let notv = invertValue v
            writePinValue p notv
            putText $ unwords ["Output:", show notv]

main :: IO ()
main =execParser opts >>= run
  where
    opts =
      info (helper <*> cmds)
           (fullDesc <>
            progDesc "Example sysfs hpio programs." <>
            header "hpio-sysfs-example - run sysfs hpio demonstrations.")
