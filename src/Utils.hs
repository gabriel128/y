{-# LANGUAGE BangPatterns #-}

module Utils where

import qualified Data.Text as T
import System.Random (RandomGen (genWord32), mkStdGen)
import System.Random.Stateful (StdGen)

randVarName :: RandomGen g => g -> (T.Text, g)
randVarName gen = go $ genWord32 gen
  where
    go (randomWord, newGen) = (T.pack $ "tmp_" <> show randomWord, newGen)

createRandomGen :: StdGen
createRandomGen = mkStdGen 42
