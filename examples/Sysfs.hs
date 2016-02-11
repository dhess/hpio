{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Options.Applicative
import System.GPIO.Linux.Sysfs.IO (SysfsIOT(..))
import System.GPIO.Linux.Sysfs.Native
import System.GPIO.Linux.Sysfs.Types
import System.GPIO.Types

data GlobalOptions =
  GlobalOptions {_cmd :: Command}

data Command
  = ListPins
  | ReadEdge ReadEdgeOptions

listPinsCmd :: Parser Command
listPinsCmd = pure ListPins

data ReadEdgeOptions =
  ReadEdgeOptions {_period :: Int
                  ,_edge :: SysfsEdge
                  ,_timeout :: Int
                  ,_outputPin :: Pin
                  ,_inputPin :: Pin}

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
runNativeSysfs act = runSysfsIOT act

run :: GlobalOptions -> IO ()
run (GlobalOptions (ReadEdge (ReadEdgeOptions period edge to inputPin outputPin))) =
  void $
    concurrently
      (runNativeSysfs $ edgeRead inputPin edge to)
      (runNativeSysfs $ driveOutput outputPin period)
run (GlobalOptions ListPins) = runNativeSysfs listPins

withPin :: Pin -> NativeSysfs a -> NativeSysfs a
withPin p block = liftIO $ bracket (runNativeSysfs $ exportPin p) (const $ runNativeSysfs $ unexportPin p) (const $ runNativeSysfs block)

listPins :: NativeSysfs ()
listPins =
  availablePins >>= \case
    [] -> liftIO $ putStrLn "No GPIO pins found on this system"
    ps -> for_ ps $ liftIO . print

edgeRead :: Pin -> SysfsEdge -> Int -> NativeSysfs ()
edgeRead p edge to =
  withPin p $
    do writePinDirection p In
       writePinEdge p edge
       forever $
         do result <- threadWaitReadPinValue' p to
            case result of
              Nothing -> liftIO $ putStrLn ("readPin timed out after " ++ show to ++ " microseconds")
              Just v -> liftIO $ putStrLn ("Input: " ++ show v)

driveOutput :: Pin -> Int -> NativeSysfs ()
driveOutput p delay =
  withPin p $
    do writePinDirection p Out
       forever $
         do liftIO $ threadDelay delay
            v <- readPinValue p
            let notv = invertValue v
            writePinValue p notv
            liftIO $ putStrLn ("Output: " ++ show notv)

main :: IO ()
main =execParser opts >>= run
  where
    opts =
      info (helper <*> cmds)
           (fullDesc <>
            progDesc "Example sysfs GPIO programs." <>
            header "gpio-sysfs-example - run sysfs GPIO demonstrations.")
