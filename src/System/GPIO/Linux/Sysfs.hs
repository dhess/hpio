-- | A 'GpioF' interpreter for Linux GPIO via sysfs.
--
-- Note that this module contains some functions which are not part of
-- the 'GpioF' DSL, but which may be useful for users of the module,
-- particularly when running 'GpioF' programs using the sysfs
-- interpreter. These functions are exported for your convenience.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module System.GPIO.Linux.Sysfs
       ( -- * The Linux sysfs GPIO interpreter
         PinDescriptor(..)
       , SysfsF
       , SysfsT
       , Sysfs
       , runSysfsT
       , runSysfs
       , runSysfs'
       , runSysfsSafe
         -- * Convenience functions
       , sysfsPath
       , exportFileName
       , unexportFileName
       , pinDirName
       , pinDirectionFileName
       , pinValueFileName
       ) where

import Prelude hiding (readFile, writeFile)
import Control.Error.Util (hushT)
import Control.Error.Script (scriptIO)
import Control.Monad (filterM, void)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Free (iterT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Char (toLower)
import Data.List (isPrefixOf, sort)
import Data.Maybe (catMaybes)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName)
import System.GPIO.Free (GpioF(..), GpioT, PinDirection(..), Pin(..), Value(..))
import qualified System.IO as IO (writeFile)
import qualified System.IO.Strict as IOS (readFile)

-- | The base path to Linux's GPIO sysfs interface.
sysfsPath :: FilePath
sysfsPath = "/sys/class/gpio"

-- | The name of the control file used to export GPIO pins via sysfs.
exportFileName :: FilePath
exportFileName = sysfsPath </> "export"

-- | The name of the control file used to "unexport" GPIO pins via
-- sysfs.
unexportFileName :: FilePath
unexportFileName = sysfsPath </> "unexport"

-- | Exporting a GPIO pin via sysfs creates a control directory
-- corresponding to that pin. 'pinDirName' gives the name of that
-- directory for a given pin number.
pinDirName :: Pin -> FilePath
pinDirName (Pin n) = sysfsPath </> ("gpio" ++ show n)

-- | Pins whose direction can be controlled via sysfs provide a
-- control file. 'pinDirectionFileName' gives the name of that
-- file. Note that for some GPIO pins, the direction cannot be set. In
-- these cases, the file named by this function does not actually
-- exist.
pinDirectionFileName :: Pin -> FilePath
pinDirectionFileName p = pinDirName p </> "direction"

-- | The name of the control file used to read and write the pin's
-- value.
pinValueFileName :: Pin -> FilePath
pinValueFileName p = pinDirName p </> "value"

-- | The sysfs interpreter's pin handle type. Currently it's just a
-- newtype wrapper around a 'Pin'. The constructor is exported for
-- convenience, but note that the implementation may change in future
-- versions of the package.
newtype PinDescriptor = PinDescriptor { pin :: Pin } deriving (Show, Eq, Ord)

-- | A monad transformer which adds Linux sysfs GPIO computations to
-- other monads.
type SysfsT = GpioT String PinDescriptor

-- | The Linux sysfs GPIO DSL type.
type SysfsF = GpioF String PinDescriptor

-- | Run a 'SysfsT' computation embedded in a 'MonadIO' monad instance
-- and return the result. Errors that occur in the computation or in
-- the interpreter are thrown with a 'String' argument via
-- 'throwError', so the wrapped monad must also be an instance of
-- 'MonadError String'. Any 'IOException's that occur as a side effect
-- of the computation are not handled here and are simply propagated
-- upwards.
--
-- (Errors that could occur in the interpreter are generally limited
-- to reading unexpected results from various sysfs GPIO control
-- files.)
runSysfsT :: (MonadError String m, MonadIO m) => SysfsT m a -> m a
runSysfsT = iterT run
  where
    run :: (MonadError String m, MonadIO m) => SysfsF (m a) -> m a

    run (Pins next) =
      do hasSysfs <- liftIO $ doesDirectoryExist sysfsPath
         case hasSysfs of
           False -> next []
           True -> allPins >>= next

    -- Export the pin. Note that it may already be exported, which we
    -- treat as success.
    run (Open p@(Pin n) next) =
      do hasSysfs <- liftIO $ doesDirectoryExist sysfsPath
         case hasSysfs of
           False -> next (Left "sysfs GPIO is not present")
           True ->
             do exported <- liftIO $ doesDirectoryExist (pinDirName p)
                case exported of
                  True -> next (Right $ PinDescriptor p)
                  False ->
                    do void $ writeFile exportFileName (show n)
                       next (Right $ PinDescriptor p)

    run (Close d next) =
      do let (Pin n) = pin d
         void $ writeFile unexportFileName (show n)
         next

    run (Direction d next) =
      do let p = pin d
         hasDirection <- liftIO $ doesFileExist (pinDirectionFileName p)
         case hasDirection of
           False -> next Nothing
           True ->
             do dir <- readFile (pinDirectionFileName p)
                case dir of
                  "in\n"  -> next $ Just In
                  "out\n" -> next $ Just Out
                  _     -> throwError $ "Unexpected direction value for " ++ show p

    run (SetDirection d dir next) =
      do let p = pin d
         void $ writeFile (pinDirectionFileName p) (lowercase $ show dir)
         next

    run (ReadPin d next) =
      do let p = pin d
         value <- readFile (pinValueFileName p)
         case value of
           "0\n" -> next Low
           "1\n" -> next High
           _   -> throwError $ "Unexpected pin value for " ++ show p

    run (WritePin d v next) =
      do let p = pin d
         void $ writeFile (pinValueFileName p) (toSysfsValue v)
         next

-- | A convenient specialization of 'SysfsT' which runs sysfs GPIO
-- computations in the 'IO' monad, and returns results as 'Either
-- String a'.
type Sysfs a = SysfsT (ExceptT String IO) a

-- | Run a 'Sysfs' computation in the 'IO' monad and return the result
-- as 'Right a'. If an error occurs in the computation or in the
-- interpreter, it is returned as 'Left String'. However, the function
-- does not catch any 'IOException's that occur as a side effect of
-- the computation. Those will propagate upwards.
runSysfs :: Sysfs a -> IO (Either String a)
runSysfs action = runExceptT $ runSysfsT action

-- | Run a 'Sysfs' computation and return the result in an 'Either
-- String a'. If an error in the computation, in the 'Sysfs'
-- interpreter, or elsewhere while the computation is running
-- (including 'IOException's that occur as a side effect of the
-- computation) it is handled by this function and is returned as an
-- error result via 'Left String'. Therefore, this function is total;
-- i.e., it always returns a result (assuming the computation
-- terminates) and never throws an exception.
runSysfsSafe :: Sysfs a -> IO (Either String a)
runSysfsSafe action =
  do result <- runExceptT $ scriptIO (runSysfs action)
     case result of
       Left e -> return $ Left e
       Right a -> return a

-- | Run a 'Sysfs' computation in the 'IO' monad. If an error occurs
-- in the computation, in the 'Sysfs' interpreter, or in the "real
-- world" (e.g., 'IO' exceptions) it will be thrown as an
-- 'IOException'. In other words, all errors will be expressed as
-- 'IOException's, just as a plain 'IO' computation would do.
runSysfs' :: Sysfs a -> IO a
runSysfs' action =
  do result <- runSysfs action
     case result of
       Left e -> fail e
       Right a -> return a


-- Helper functions that aren't exported.
--

toSysfsValue :: Value -> String
toSysfsValue Low = "0"
toSysfsValue High = "1"

lowercase :: String -> String
lowercase = fmap toLower

writeFile :: (MonadIO m) => FilePath -> String -> m ()
writeFile f s = liftIO $ IO.writeFile f s

readFile :: (MonadIO m) => FilePath -> m String
readFile f = liftIO $ IOS.readFile f

readFromFile :: (MonadIO m, Read a) => FilePath -> m a
readFromFile f = liftIO (IOS.readFile f >>= readIO)

maybeIO :: (MonadIO m) => IO a -> MaybeT m a
maybeIO = hushT . scriptIO

chipBaseGpio :: (MonadIO m) => FilePath -> m Int
chipBaseGpio chipDir = readFromFile (chipDir </> "base")

chipNGpio :: (MonadIO m) => FilePath -> m Int
chipNGpio chipDir = readFromFile (chipDir </> "ngpio")

allPins :: (MonadIO m) => m [Pin]
allPins =
  do sysfsEntries <- liftIO $ getDirectoryContents sysfsPath
     let sysfsContents = fmap (sysfsPath </>) sysfsEntries
     sysfsDirectories <- filterM (liftIO . doesDirectoryExist) sysfsContents
     let chipDirs = filter (\f -> isPrefixOf "gpiochip" $ takeFileName f) sysfsDirectories
     maybePins <- mapM (runMaybeT . pinRange) chipDirs
     return $ sort $ concat $ catMaybes maybePins

pinRange :: (MonadIO m) => FilePath -> MaybeT m [Pin]
pinRange chipDir =
  do base <- maybeIO $ chipBaseGpio chipDir
     ngpio <- maybeIO $ chipNGpio chipDir
     case (base >= 0 && ngpio > 0) of
       False -> return []
       True -> return $ fmap Pin [base .. (base + ngpio - 1)]
