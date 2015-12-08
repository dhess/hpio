{-# LANGUAGE FlexibleContexts #-}

-- Compile from top-level with "ghc -isrc"

module Main where

import Control.Monad (void)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.GPIO.Free
import System.GPIO.Linux.Sysfs (runSysfsT)

output :: (MonadIO m) => String -> m ()
output = liftIO . putStrLn

example :: (MonadIO m, MonadError e m) => GpioT e h m ()
example =
  do availablePins <- pins
     output ("GPIO pins available: " ++ show availablePins)
     case availablePins of
       [] -> output "No pins available on this system, exiting."
       p:_ ->
         do output ("Opening " ++ show p)
            result <- open p
            case result of
              Left e -> throwError e
              Right h ->
                do val <- readPin h
                   output ("Pin value is " ++ show val)
                   maybeDir <- direction h
                   case maybeDir of
                     Nothing ->
                       do output "Pin direction cannot be set"
                          output ("Closing " ++ show p)
                          void $ close h
                          return ()
                     Just In ->
                       do output "Pin direction is 'in'"
                          output "Setting pin direction to 'out'"
                          void $ setDirection h Out
                     Just Out ->
                       output "Pin direction is 'out'"
                   case val of
                     High ->
                       do output "Writing pin value 'low'"
                          void $ writePin h Low
                     Low ->
                       do output "Writing pin value 'high'"
                          void $ writePin h High
                   output ("Closing " ++ show p)
                   close h

main :: IO ()
main =
  do result <- runExceptT $ runSysfsT example
     case result of
       Left e -> putStrLn ("Error: " ++ e)
       _ -> putStrLn "OK"


