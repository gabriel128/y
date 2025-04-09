{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module AtomizeTests (test_id_ir) where

import Ast.Ast
import Context (Context (..), defaultContext)
import Data.Either.Combinators
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import EffUtils (runStateErrorEff)
import Parser.Parser
import Passes.Atomizer
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- import Test.Tasty.SmallCheck as SC

test_id_ir :: TestTree
test_id_ir = testGroup "Tests" [unitTests]

runComplexStmts :: [Stmt] -> (Context, Program)
runComplexStmts = fromRight' . run
  where
    run :: [Stmt] -> Either Text (Context, Program)
    run stmts = runStateErrorEff defaultContext (removeComplexStmts (newProgram stmts))

liftEither :: (Monad m, MonadFail m) => Either Text t -> m t
liftEither (Right val) = return val
liftEither (Left err) = fail . unpack $ err

unitTests :: TestTree
unitTests =
  testGroup
    "ListStack"
    [ testCase "remove complex ops do nothing when not needed" $ do
        (Context locals 0, prog) <- liftEither $ runComplexStmts . progStmts <$> runProgramParser "x : i64 = 8;"
        expected_prog <- liftEither $ runProgramParser "x : i64 = 8;"
        assertEqual "" locals (Set.fromList ["x"])
        assertEqual "" expected_prog prog,
      -- --
      testCase "remove complex ops do nothing with more complex ops but still atomic" $ do
        (Context locals 0, prog) <- liftEither $ runComplexStmts . progStmts <$> runProgramParser "x : i64 = -8;"
        expected <- liftEither $ runProgramParser "x : i64 = -8;"
        assertEqual "" (Set.fromList ["x"]) locals
        assertEqual "" expected prog,
      -- --
      testCase "remove complex ops on Unary" $ do
        (Context locals 0, prog) <- liftEither $ runComplexStmts . progStmts <$> runProgramParser "x : i64 = -(-8);"
        let [var', "x"] = Set.toAscList locals
        expected_prog <- liftEither $ runProgramParser "tmp_0 = -8; x : i64 = -tmp_0;"
        assertEqual "" expected_prog prog
        assertEqual "" "tmp_0" var',
      -- --
      testCase "multiple stmts remove complex ops with Unary" $ do
        (Context locals 0, prog) <- liftEither $ runComplexStmts . progStmts <$> runProgramParser "x : i64 = -(-8); y : i64 = -(-8);"
        let [_var, _var', "x", "y"] = Set.toAscList locals
        expected_prog <- liftEither $ runProgramParser "tmp_0 = -8; x : i64 = -tmp_0; tmp_1 = -8; y : i64 = -tmp_1;"
        assertEqual "" expected_prog prog,
      -- --
      testCase "remove nested Unary" $ do
        (Context locals 0, prog) <- liftEither $ runComplexStmts . progStmts <$> runProgramParser "x : i64 = -(-(-8));"
        let [_var, _var', "x"] = Set.toAscList locals
        expected_prog <- liftEither $ runProgramParser "tmp_1 = -8; tmp_0 = -tmp_1; x : i64 = -tmp_0;"
        assertEqual "" expected_prog prog,
      -- --
      testCase "remove complex ops on Binary ops" $ do
        (Context locals 0, prog) <- liftEither $ runComplexStmts . progStmts <$> runProgramParser "x : i64 = 10 + -8;"
        let [_var, "x"] = Set.toAscList locals
        expected_prog <- liftEither $ runProgramParser "tmp_0 = -8; x : i64 = 10 + tmp_0;"
        assertEqual "" expected_prog prog,
      -- --
      testCase "multiple similar bindings discard first one" $ do
        (Context locals 0, prog) <- liftEither $ runComplexStmts . progStmts <$> runProgramParser "x = 10 + 8; x = 11 + 9;"
        expected_prog <- liftEither $ runProgramParser "x  = 10 + 8; x = 11 + 9;"
        assertEqual "" ["x"] (Set.toAscList locals)
        assertEqual "" expected_prog prog,
      -- --
      testCase "remove expr on Print" $ do
        (Context locals 0, prog) <- liftEither $ runComplexStmts . progStmts <$> runProgramParser "print(10 + 8);"
        expected_prog <- liftEither $ runProgramParser "tmp_0 = 10 + 8; print(tmp_0);"
        assertEqual "" ["tmp_0"] (Set.toAscList locals)
        assertEqual "" expected_prog prog,
      -- ---
      testCase "remove complex expr on Return" $ do
        (Context locals 0, prog) <- liftEither $ runComplexStmts . progStmts <$> runProgramParser "return 10 + 8;"
        expected_prog <- liftEither $ runProgramParser "tmp_0 = 10 + 8; return tmp_0;"
        assertEqual "" ["tmp_0"] (Set.toAscList locals)
        assertEqual "" expected_prog prog,
      -- --
      testCase "remove more complex ops on Binary ops" $ do
        (Context locals 0, prog) <- liftEither $ runComplexStmts . progStmts <$> runProgramParser "x = -10 + 8; "
        expected_prog <- liftEither $ runProgramParser "tmp_0 = -10; x = tmp_0 + 8;"
        assertEqual "" ["tmp_0", "x"] (Set.toAscList locals)
        assertEqual "" expected_prog prog,
      -- -- -- --
      testCase "remove even more complex ops on Binary ops" $ do
        (Context locals 0, prog) <- liftEither $ runComplexStmts . progStmts <$> runProgramParser "x = -10 + -8; "
        expected_prog <- liftEither $ runProgramParser "tmp_0 = -10; tmp_1 = -8; x = tmp_0 + tmp_1;"
        assertEqual "" ["tmp_0", "tmp_1", "x"] (Set.toAscList locals)
        assertEqual "" expected_prog prog
    ]
