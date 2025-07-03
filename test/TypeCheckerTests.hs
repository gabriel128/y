{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module TypeCheckerTests (test_type_checking) where

import Ast.Ast
import Context (Context, defaultContext)
import qualified Data.Bifunctor
import Data.Either.Combinators
import Data.Text
import EffUtils (runStateErrorEff)
import Parser.Parser
import Passes.TypeChecker
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Types.Defs
import Utils

-- import Types.Defs

-- import Test.Tasty.SmallCheck as SC

test_type_checking :: TestTree
test_type_checking = testGroup "TypeChecker" unitTests

runTypeCheck :: Program Expr (Maybe Type) -> Either Text (Context, [Stmt Expr Type])
runTypeCheck program =
    fmap (Data.Bifunctor.second progStmts) $ runStateErrorEff defaultContext $ typeCheck program

unitTests :: [TestTree]
unitTests =
    [ --
      testCase "Type check simple assignment" $ do
        tProg <- liftEither $ runProgramParser "x : u64 = 8;"
        (_ctx, _stmts) <- liftEither $ runTypeCheck tProg
        assertBool "" True
    , -- --
      testCase "Type inference" $ do
        _prog <- liftEither $ runTypeCheck <$> runProgramParser "x = 8;"
        assertBool "" True
    , testCase "Defines right hand side type if constant number" $ do
        res <- liftEither $ runTypeCheck <$> runProgramParser "x : u64 = 8; "
        (_, stmts) <- liftEither res
        let expected = [Let U64 "x" (Lit (LNum U64 8))]
        assertEqual "" stmts expected
    , -- --
      testCase "Type checks same type lhs rhs on binops sides" $ do
        progErr <- liftEither $ runTypeCheck <$> runProgramParser "x : u64 = 8; y : i64 = 0; z = x + y;"
        -- putStrLn (show progErr)
        assertBool "" (isLeft progErr)
    , -- --
      testCase "Type checks division by 0" $ do
        prog <- liftEither $ runTypeCheck <$> runProgramParser "x = 8 / 0;"
        assertBool "" (isLeft prog)
    ]
