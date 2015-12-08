{-# LANGUAGE FlexibleContexts #-}

-- Compile from top-level with "ghc -isrc"

module Main where

import Control.Monad (void, when)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.GPIO.Free
import System.GPIO.Linux.Sysfs (runSysfsT)

output :: (MonadIO m) => String -> m ()
output = liftIO . putStrLn

toggleValue :: Value -> Value
toggleValue High = Low
toggleValue Low = High

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
                     Nothing -> output "Pin direction cannot be set"
                     Just dir ->
                       do output $ "Pin direction is " ++ show dir
                          when (dir == In) $
                            do output "Setting pin direction to 'out'"
                               void $ setDirection h Out
                          let newValue = toggleValue val
                          output $ "Setting pin value to " ++ show newValue
                          void $ writePin h newValue
                   output ("Closing " ++ show p)
                   close h

main :: IO ()
main =
  do result <- runExceptT $ runSysfsT example
     case result of
       Left e -> putStrLn ("Error: " ++ e)
       _ -> putStrLn "OK"


