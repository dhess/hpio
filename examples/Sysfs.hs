{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Data.Foldable (for_)
import Options.Applicative
import System.GPIO.Linux.Sysfs.IO
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

run :: GlobalOptions -> IO ()
run (GlobalOptions (ReadEdge (ReadEdgeOptions period edge to inputPin outputPin))) =
  do void $ forkIO (edgeRead inputPin edge to)
     driveOutput outputPin period
run (GlobalOptions ListPins) = listPins

withPin :: Pin -> IO a -> IO a
withPin p block = bracket (exportPin p) (const $ unexportPin p) (const block)

listPins :: IO ()
listPins =
  availablePins >>= \case
    [] -> putStrLn "No GPIO pins found on this system"
    ps -> for_ ps print

edgeRead :: Pin -> SysfsEdge -> Int -> IO ()
edgeRead p edge to =
  withPin p $
    do writePinDirection p In
       writePinEdge p edge
       forever $
         do result <- threadWaitReadPinValue' p to
            case result of
              Nothing -> putStrLn ("readPin timed out after " ++ show to ++ " microseconds")
              Just v -> putStrLn ("Input: " ++ show v)

driveOutput :: Pin -> Int -> IO ()
driveOutput p delay =
  withPin p $
    do writePinDirection p Out
       forever $
         do threadDelay delay
            v <- readPinValue p
            let notv = invertValue v
            writePinValue p notv
            putStrLn ("Output: " ++ show notv)

main :: IO ()
main =execParser opts >>= run
  where
    opts =
      info (helper <*> cmds)
           (fullDesc <>
            progDesc "Example sysfs GPIO programs." <>
            header "gpio-sysfs-example - run sysfs GPIO demonstrations.")
