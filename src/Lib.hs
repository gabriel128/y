module Lib where

import Ast.Ast
import Control.Algebra
-- import Control.Monad.Error.Class (liftEither)
-- import Control.Monad.Except (MonadError)
-- import Control.Monad.Reader

import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.Lift
import Control.Carrier.State.Strict (runState)
import Control.Effect.Error
import Control.Effect.State as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Irs.Atomize as Atomize
import qualified Irs.X86Var as X86Var

--  Example of fused effects usage
-- data Context = Context {getA :: Int, getB :: Text} deriving (Show)

-- ex3 :: (Has (State Context) sig m, Has (State Int) sig m, Has (Throw Text) sig m, Has Fresh sig m) => m Int
-- ex3 = do
--   s <- get @Context
--   s' <- get @Int
--   -- _x <- throwError (T.pack "nope")
--   y <- fresh
--   z <- fresh
--   put (Context (getA s + y) (T.pack (show s' <> " " <> show z)))
--   pure 3

-- p :: Algebra sig m => m (Either Text (Context, (Int, (Int, Int))))
-- p = runError . runState @Context (Context 0 "ouch") . runState @Int 9 . runFresh 0 $ ex3

-- printP :: IO ()
-- printP = do
--   a <- runM p
--   case a of
--     Left err -> print err
--     Right (ctx, _) -> print ctx

-- compile :: Program -> Either Text Text
-- compile (Program info stmts) =
--   do
--     let atomized = Atomize.removeComplexStmts makeDefaultInfo stmts
--     x86Var <- X86Var.fromAst atomized
--     pure ""
