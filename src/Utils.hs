{-# LANGUAGE BangPatterns #-}

module Utils where

import qualified Data.Text as T
import System.Random (RandomGen (genWord32), mkStdGen)
import System.Random.Stateful (StdGen)

freshVarName :: (Monad m) => m Int -> m T.Text
freshVarName = fmap (\x -> T.pack $ "tmp_" <> show x)

randVarName :: RandomGen g => g -> (T.Text, g)
randVarName gen = go $ genWord32 gen
  where
    go (randomWord, newGen) = (T.pack $ "tmp_" <> show randomWord, newGen)

createRandomGen :: StdGen
createRandomGen = mkStdGen 42
