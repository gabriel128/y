{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module LocalMtl where

import System.Process (readProcess)

---

class MonadT t where
  liftT :: Monad m => m a -> t m a

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

instance MonadT (EitherT e) where
  liftT a = EitherT $ fmap Right a

class Monad m => MonadEither e m | m -> e where
  throwError :: e -> m a

instance Monad m => MonadEither e (EitherT e m) where
  throwError err = EitherT $ pure (Left err)

class Monad m => MonadTIO m where
  liftTIO :: IO a -> m a

instance MonadTIO IO where
  liftTIO = id

instance MonadTIO m => MonadTIO (EitherT e m) where
  liftTIO = liftT . liftTIO

---

class MonadFiles m where
  writeToFile :: FilePath -> String -> m ()
  readFromFile :: FilePath -> m String

class MonadProcess m where
  readFromProcess :: FilePath -> [String] -> String -> m String

class MonadPrinter m where
  printLn :: String -> m ()

instance MonadPrinter IO where
  printLn xs = putStrLn xs

instance (MonadTIO m, MonadT t) => MonadPrinter (t m) where
  printLn xs = liftT . liftTIO $ putStrLn xs

instance (MonadTIO m, MonadT t) => MonadProcess (t m) where
  readFromProcess filePath cmds stdio = liftT . liftTIO $ readProcess filePath cmds stdio

instance (MonadTIO m, MonadT t) => MonadFiles (t m) where
  writeToFile path text = liftT . liftTIO $ writeFile path text
  readFromFile path = liftT . liftTIO $ readFile path

liftEither :: MonadEither e m => Either e a -> m a
liftEither (Right a) = pure a
liftEither (Left e) = throwError e
