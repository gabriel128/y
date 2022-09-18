module Mtl where

import Control.Monad.Trans
import System.Process (readProcess)

class MonadFiles m where
  writeToFile :: FilePath -> String -> m ()
  readFromFile :: FilePath -> m String

class MonadProcess m where
  readFromProcess :: FilePath -> [String] -> String -> m String

class MonadPrinter m where
  printf :: String -> m ()

instance (MonadIO m) => MonadPrinter m where
  printf xs = liftIO $ putStrLn xs

instance (MonadIO m) => MonadProcess m where
  readFromProcess filePath cmds stdio = liftIO $ readProcess filePath cmds stdio

instance (MonadIO m) => MonadFiles m where
  writeToFile path text = liftIO $ writeFile path text
  readFromFile path = liftIO $ readFile path
