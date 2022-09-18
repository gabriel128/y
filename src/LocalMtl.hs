{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module LocalMtl where

import Control.Monad.Trans
import System.Process (readProcess)

class MonadFiles m where
  writeToFile :: FilePath -> String -> m ()
  readFromFile :: FilePath -> m String

class MonadProcess m where
  readFromProcess :: FilePath -> [String] -> String -> m String

class MonadPrinter m where
  printLn :: String -> m ()

instance MonadPrinter IO where
  printLn xs = putStrLn xs

instance (MonadIO m, MonadTrans t) => MonadPrinter (t m) where
  printLn xs = lift . liftIO $ putStrLn xs

instance (MonadIO m, MonadTrans t) => MonadProcess (t m) where
  readFromProcess filePath cmds stdio = lift . liftIO $ readProcess filePath cmds stdio

instance (MonadIO m, MonadTrans t) => MonadFiles (t m) where
  writeToFile path text = lift . liftIO $ writeFile path text
  readFromFile path = lift . liftIO $ readFile path

---

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f eitherTa =
    let ma = runEitherT eitherTa in EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ pure (Right a)
  fab <*> eb =
    let f = runEitherT fab
        e = runEitherT eb
     in EitherT $ (<*>) <$> f <*> e

instance Monad m => Monad (EitherT e m) where
  (EitherT me) >>= f = EitherT $ do
    ma <- me
    case ma of
      (Right a) -> runEitherT $ f a
      (Left e) -> pure (Left e)

class Monad m => MonadEither m e | m -> e where
  throwError :: e -> m a

class MonadT t where
  liftT :: Monad m => m a -> t m a

class Monad m => MonadTIO m where
  liftTIO :: IO a -> m a

instance MonadTIO IO where
  liftTIO = id
