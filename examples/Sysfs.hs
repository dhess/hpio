{-# LANGUAGE FlexibleContexts #-}

-- Compile from top-level with "ghc -isrc"

module Main where

import Control.Error.Util (errLn)
import Control.Monad (when)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Exit (exitFailure)
import System.GPIO.Free
import System.GPIO.Linux.SysfsIO (runSysfsIOSafe)
import System.GPIO.Types

output :: (MonadIO m) => String -> m ()
output = liftIO . putStrLn

pickAPin :: (MonadIO m) => GpioT e h m m Pin
pickAPin =
  do availablePins <- pins
     output ("GPIO pins available: " ++ show availablePins)
     case availablePins of
       [] ->
         do output "Can't find any obviously available pins on this system, let's try Pin 0."
            return $ Pin 0
       p:_ -> return p

example :: (MonadIO m, MonadError e m) => GpioT e h m m ()
example =
  do p <- pickAPin
     output ("Opening " ++ show p)
     withPin p $ \h ->
       do val <- readPin h
          output ("Pin value is " ++ show val)
          maybeDir <- getPinDirection h
          case maybeDir of
            Nothing -> output "Pin direction cannot be set"
            Just dir ->
              do output $ "Pin direction is " ++ show dir
                 when (dir == In) $
                   do output $ "Setting pin direction to " ++ show Out
                      setPinDirection h Out
                 newValue <- togglePinValue h
                 output $ "Setting pin value to " ++ show newValue
                 writePin h newValue

main :: IO ()
main =
  do result <- runSysfsIOSafe example
     case result of
       Left e ->
         do errLn $ "Error: " ++ e
            exitFailure
       Right _ -> return ()
