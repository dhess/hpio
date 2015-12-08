{-# LANGUAGE FlexibleContexts #-}

-- Compile from top-level with "ghc -isrc"

module Main where

import Control.Error.Script (runScript, scriptIO)
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
                            do output $ "Setting pin direction to " ++ show Out
                               void $ setDirection h Out
                          let newValue = toggleValue val
                          output $ "Setting pin value to " ++ show newValue
                          void $ writePin h newValue
                   output ("Closing " ++ show p)
                   close h

-- Note the approach to error handling here. The 'example' program
-- above only cares that the monad in which it runs is an instance of
-- 'MonadError e m', but otherwise is agnostic about error handling.
--
-- Due to the use of 'runExceptT' here, errors that occur in the
-- example program are expressed as 'Left String' and propagated
-- upwards as 'userError's, as will exceptions that occur in the
-- 'runSysfsT' interpreter, due to the fact that 'runSysfsT' manifests
-- exceptions as 'String'.
--
-- Errors that occur due to real-world issues (i.e., IO exceptions)
-- will not be handled by this function. It's the job of the caller to
-- decide what to do with those.
--
-- So, in summary: program errors or interpreter errors are handled
-- here and punted upwards. Real-world errors pass through.
runIO :: IO ()
runIO =
  do result <- runExceptT $ runSysfsT example
     case result of
       Left e -> fail $ "runIO caught error: " ++ e
       Right _ -> return ()

-- All errors will be handled by printing the first error to stderr.
-- This includes program errors, interpreter errors, and IO errors.
main :: IO ()
main = runScript $ scriptIO runIO

-- If you want to run the example with unhandled exceptions, use this
-- version:

-- main :: IO ()
-- main = runIO
