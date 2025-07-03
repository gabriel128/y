{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module AtomizerTests (test_typed_atomizer) where

import Ast.Ast
import Context (Context (..), defaultContext)
import Data.Either.Combinators
import qualified Data.Set as Set
import Data.Text (Text)
import EffUtils (runStateErrorEff)
import Irs.AtomIr
import Parser.Parser
import Passes.Atomizer
import Passes.TypeChecker
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Types.Defs
import Utils

-- import Test.Tasty.SmallCheck as SC

test_typed_atomizer :: TestTree
test_typed_atomizer = testGroup "TypedAtomizer" unitTests

runComplexStmts :: [Stmt Expr Type] -> (Context, Program AExpr Type)
runComplexStmts = fromRight' . run
  where
    run :: [Stmt Expr Type] -> Either Text (Context, Program AExpr Type)
    run stmts = runStateErrorEff defaultContext (removeComplexStmts (newProgram stmts))

-- We only test valid programs
runTypeCheck :: Program Expr (Maybe Type) -> Either Text (Context, Program Expr Type)
runTypeCheck program =
    runStateErrorEff defaultContext $ typeCheck program

getTypeCheckedProg :: Text -> Either Text (Program Expr Type)
getTypeCheckedProg text = do
    tProg <- runProgramParser text
    (_ctx, prog) <- runTypeCheck tProg
    return prog

unitTests :: [TestTree]
unitTests =
    [ testCase "remove complex ops do nothing when not needed" $ do
        prog <- liftEither $ getTypeCheckedProg "x : u64 = 8;"
        expectedProg <- liftEither $ getTypeCheckedProg "x : u64 = 8;"
        let (Context locals 0, prog') = runComplexStmts (progStmts prog)
        let (Context _ 0, expectedProg') = runComplexStmts (progStmts expectedProg)
        assertEqual "" locals (Set.fromList ["x"])
        assertEqual "" expectedProg' prog'
    , -- -- --
      testCase "remove complex ops on Unary" $ do
        prog <- liftEither $ getTypeCheckedProg "x : i64 = -(-8);"
        expectedProg <- liftEither $ getTypeCheckedProg "tmp_0 : i64 = -8; x : i64 = -tmp_0;"
        let (Context locals 0, prog') = runComplexStmts (progStmts prog)
        let (Context _ 0, expectedProg') = runComplexStmts (progStmts expectedProg)
        let [var', "x"] = Set.toAscList locals
        assertEqual "" expectedProg' prog'
        assertEqual "" "tmp_0" var'
    ]

-- \| Now the typechecker ensures that the expressions are in TAC
--   so we don't need these anymore

--   testCase "remove complex ops do nothing with more complex ops but still atomic" $ do
--     prog <- liftEither $ getTypeCheckedProg "x : i64 = -8;"
--     expectedProg <- liftEither $ getTypeCheckedProg "x : i64 = -8;"
--     let (Context locals 0, prog') = runComplexStmts (progStmts prog)
--     assertEqual "" (Set.fromList ["x"]) locals
--     assertEqual "" expectedProg prog'
-- --
-- , -- -- --
--   testCase "multiple stmts remove complex ops with Unary" $ do
--     prog <- liftEither $ getTypeCheckedProg "x : i64 = -(-8); y : i64 = -(-8);"
--     expectedProg <-
--         liftEither $
--             getTypeCheckedProg
--                 "tmp_0 : i64 = -8; x : i64 = -tmp_0; tmp_1 : i64 = -8; y : i64 = -tmp_1;"
--     let (Context locals 0, prog') = runComplexStmts (progStmts prog)
--     let [_var, _var', "x", "y"] = Set.toAscList locals
--     assertEqual "" expectedProg prog'
-- , -- -- --
--   testCase "remove nested Unary" $ do
--     prog <- liftEither $ getTypeCheckedProg "x : i64 = -(-(-8));"
--     expectedProg <- liftEither $ getTypeCheckedProg "tmp_1 = -8; tmp_0 = -tmp_1; x : i64 = -tmp_0;"
--     let (Context locals 0, prog') = runComplexStmts (progStmts prog)
--     let [_var, _var', "x"] = Set.toAscList locals
--     assertEqual "" expectedProg prog'
-- , -- -- --
--   testCase "remove complex ops on Binary ops" $ do
--     prog <- liftEither $ getTypeCheckedProg "x : i64 = 10 + -8;"
--     expectedProg <- liftEither $ getTypeCheckedProg "tmp_0 : i64 = -8; x : i64 = 10 + tmp_0;"
--     let (Context locals 0, prog') = runComplexStmts (progStmts prog)
--     let [_var, "x"] = Set.toAscList locals
--     assertEqual "" expectedProg prog'
-- , -- -- --
--   testCase "multiple similar bindings discard first one" $ do
--     prog <- liftEither $ getTypeCheckedProg "x = 10 + 8; x = 11 + 9;"
--     expectedProg <- liftEither $ getTypeCheckedProg "x  = 10 + 8; x = 11 + 9;"
--     let (Context locals 0, prog') = runComplexStmts (progStmts prog)
--     assertEqual "" ["x"] (Set.toAscList locals)
--     assertEqual "" expectedProg prog'
-- , -- -- --
--   testCase "remove expr on Print" $ do
--     prog <- liftEither $ getTypeCheckedProg "print(10 + 8);"
--     expectedProg <- liftEither $ getTypeCheckedProg "tmp_0 = 10 + 8; print(tmp_0);"
--     let (Context locals 0, prog') = runComplexStmts (progStmts prog)
--     assertEqual "" ["tmp_0"] (Set.toAscList locals)
--     assertEqual "" expectedProg prog'
-- , -- -- ---
--   testCase "remove complex expr on Return" $ do
--     prog <- liftEither $ getTypeCheckedProg "return 10 + 8;"
--     expectedProg <- liftEither $ getTypeCheckedProg "tmp_0 = 10 + 8; return tmp_0;"
--     let (Context locals 0, prog') = runComplexStmts (progStmts prog)
--     assertEqual "" ["tmp_0"] (Set.toAscList locals)
--     assertEqual "" expectedProg prog'
-- , -- -- --
--   testCase "remove more complex ops on Binary ops" $ do
--     prog <- liftEither $ getTypeCheckedProg "x = -10 + 8; "
--     expectedProg <- liftEither $ getTypeCheckedProg "tmp_0 = -10; x = tmp_0 + 8;"
--     let (Context locals 0, prog') = runComplexStmts (progStmts prog)
--     assertEqual "" ["tmp_0", "x"] (Set.toAscList locals)
--     assertEqual "" expectedProg prog'
-- , -- -- -- --
--   testCase "remove even more complex ops on Binary ops" $ do
--     prog <- liftEither $ getTypeCheckedProg "x = -10 + -8; "
--     expectedProg <- liftEither $ getTypeCheckedProg "tmp_0 = -10; tmp_1 = -8; x = tmp_0 + tmp_1;"
--     let (Context locals 0, prog') = runComplexStmts (progStmts prog)
--     assertEqual "" ["tmp_0", "tmp_1", "x"] (Set.toAscList locals)
--     assertEqual "" expectedProg prog'
