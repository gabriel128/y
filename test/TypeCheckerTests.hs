{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module TypeCheckerTests (test_type_checking) where

import Ast.Ast
import Ast.TypedAst
import Context (Context, defaultContext)
import qualified Data.Bifunctor
import Data.Either.Combinators
import Data.Text (Text, unpack)
import EffUtils (runStateErrorEff)
import Parser.Parser
import Passes.TypeChecker
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Utils

-- import Types.Defs

-- import Test.Tasty.SmallCheck as SC

test_type_checking :: TestTree
test_type_checking = testGroup "TypeChecker" unitTests

runTypeCheck :: TypedProgram -> Either Text (Context, [Stmt])
runTypeCheck program =
  fmap (Data.Bifunctor.second progStmts) $ runStateErrorEff defaultContext $ typeCheck program

unitTests :: [TestTree]
unitTests =
  [ --
    testCase "Type check simple assignment" $ do
      tProg <- liftEither $ runProgramParser "x : u64 = 8;"
      (_ctx, _stmts) <- liftEither $ runTypeCheck tProg
      assertBool "" True,
    -- --
    testCase "Type inference" $ do
      _prog <- liftEither $ runTypeCheck <$> runProgramParser "x = 8;"
      assertBool "" True
  ]
