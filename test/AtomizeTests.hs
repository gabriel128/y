{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AtomizeTests (test_id_ir) where

import Ast.Ast
import Data.Either.Combinators
import Data.Text (Text, pack)
import Irs.Atomize
import Irs.PassEffs
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

-- import Test.Tasty.SmallCheck as SC

test_id_ir :: TestTree
test_id_ir = testGroup "Tests" [unitTests]

expr :: Expr
expr =
  let negEight = UnaryOp Neg (Const 8)
      ast1_1 = BinOp Add (Const 42) negEight
   in ast1_1

runComplexStmts :: [Stmt] -> Program
runComplexStmts stmts = fromRight' $ runStErr defaultInfo (removeComplexStmts (newProgram stmts))

unitTests :: TestTree
unitTests =
  testGroup
    "ListStack"
    [ testCase "remove complex ops do nothing when not needed" $
        let prog = runComplexStmts [Let "x" (Const 8)]
            expected = Program [Let "x" (Const 8)]
         in assertEqual "" prog expected,
      --
      testCase "remove complex ops do nothing with more complex ops but still atomic" $
        let prog = runComplexStmts [Let "x" (UnaryOp Neg (Const 8))]
            expected = newProgram [Let "x" (UnaryOp Neg (Const 8))]
         in assertEqual "" prog expected
        -- --
        -- testCase "remove complex ops on Unary" $
        --   case removeComplexStmts makeDefaultInfo [Let "x" (UnaryOp Neg (UnaryOp Neg (Const 8)))] of
        --     Program
        --       (Info [var'] 0)
        --       [ Let var (UnaryOp Neg (Const 8)),
        --         Let "x" (UnaryOp Neg (Var var1))
        --         ] -> assertBool "tmp vars are not equal" (var == var1 && var' == var)
        --     res -> assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
        -- --
        -- testCase "multiple stmts remove complex ops with Unary" $
        --   case removeComplexStmts
        --     makeDefaultInfo
        --     [ Let "x" (UnaryOp Neg (UnaryOp Neg (Const 8))),
        --       Let "y" (UnaryOp Neg (UnaryOp Neg (Const 8)))
        --     ] of
        --     Program
        --       (Info [var, var1] 0)
        --       [ Let var' (UnaryOp Neg (Const 8)),
        --         Let "x" (UnaryOp Neg (Var var'')),
        --         Let var1' (UnaryOp Neg (Const 8)),
        --         Let "y" (UnaryOp Neg (Var var1''))
        --         ] -> assertBool ("tmp vars are not equal: " <> show var <> show var1) (var == var' && var' == var'' && var1 == var1' && var1' == var1'' && var /= var1)
        --     res -> assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
        -- -- --
        -- testCase "remove nested Unary" $
        --   case progStmts $ removeComplexStmts makeDefaultInfo [Let "x" (UnaryOp Neg (UnaryOp Neg (UnaryOp Neg (Const 8))))] of
        --     res@[Let var (UnaryOp Neg (Const 8)), Let var1 (UnaryOp Neg (Var var')), Let "x" (UnaryOp Neg (Var var1'))] -> do
        --       assertBool ("tmp vars are not equal, got" <> show res) (var == var' && var1 == var1')
        --       assertBool ("tmp vars should be different, got: " <> show res) (var /= var1)
        --     res -> assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
        -- -- --
        -- testCase "remove complex ops on Binary ops" $
        --   case progStmts $ removeComplexStmts makeDefaultInfo [Let "x" (BinOp Add (Const 10) (UnaryOp Neg (Const 8)))] of
        --     [Let var (UnaryOp Neg (Const 8)), Let "x" (BinOp Add (Const 10) (Var var1))] -> assertBool "tmp vars are not equal" (var == var1)
        --     res -> assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
        -- -- --
        -- testCase "remove more complex ops on Binary ops" $
        --   case progStmts $ removeComplexStmts makeDefaultInfo [Let "x" (BinOp Add (UnaryOp Neg (Const 8)) (Const 10))] of
        --     [Let var (UnaryOp Neg (Const 8)), Let "x" (BinOp Add (Var var1) (Const 10))] -> assertBool "tmp vars are not equal" (var == var1)
        --     res -> assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
        -- -- --
        -- testCase "remove even more complex ops on Binary ops" $
        --   case progStmts $ removeComplexStmts makeDefaultInfo [Let "x" (BinOp Add (UnaryOp Neg (Const 8)) (UnaryOp Neg (Const 10)))] of
        --     res@[Let var (UnaryOp Neg (Const 8)), Let var1 (UnaryOp Neg (Const 10)), Let "x" (BinOp Add (Var var') (Var var1'))] -> do
        --       assertBool ("tmp vars are not equal, got" <> show res) (var == var' && var1 == var1')
        --       assertBool ("tmp vars should be different, got: " <> show res) (var /= var1)
        --     res -> assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False
    ]
