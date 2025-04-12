module Utils where

import Data.Text (Text, pack, unpack)
import System.Random (RandomGen (genWord32), mkStdGen)
import System.Random.Stateful (StdGen)

class Print s where
  textPrint :: s -> Text

instance Print Int where
  textPrint = pack . show

instance Print String where
  textPrint = pack

instance Print Text where
  textPrint a = a

freshVarName :: (Monad m) => m Int -> m Text
freshVarName = fmap (\x -> pack $ "tmp_" <> show x)

randVarName :: RandomGen g => g -> (Text, g)
randVarName gen = go $ genWord32 gen
  where
    go (randomWord, newGen) = (pack $ "tmp_" <> show randomWord, newGen)

createRandomGen :: StdGen
createRandomGen = mkStdGen 42

liftEither :: (Monad m, MonadFail m) => Either Text t -> m t
liftEither (Right val) = return val
liftEither (Left err) = fail . unpack $ err
