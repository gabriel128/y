module IdTests (test_id_ir) where

import Ast.Ast
import Ast.Examples
import Data.Map
import Interpreter.Eval
import Irs.Id
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

-- import Test.Tasty.SmallCheck as SC

test_id_ir :: TestTree
test_id_ir = testGroup "Tests" [unitTests]

expr :: Expr
expr =
  let negEight = UnaryOp Neg (Const 8)
      ast1_1 = BinOp Add (Const 42) negEight
   in ast1_1

unitTests =
  testGroup
    "ListStack"
    [ testCase "remove complex ops do nothing when not needed" $
        removeComplexStmt (Let "x" (Const 8)) @?= [Let "x" (Const 8)],
      --
      testCase "remove complex ops on Unary" $
        case removeComplexStmt (Let "x" (UnaryOp Neg (Const 8))) of
          [Let var (UnaryOp Neg (Const 8)), Let "x" (Var var1)] -> assertBool "tmp vars are not equal" (var == var1)
          _ -> assertBool "It didn't construct the correct tmp vars" False,
      --
      testCase "remove complex ops on Binary ops" $
        case removeComplexStmt (Let "x" (BinOp Add (Const 10) (Const 8))) of
          [Let var (BinOp Add (Const 10) (Const 8)), Let "x" (Var var1)] -> assertBool "tmp vars are not equal" (var == var1)
          res -> assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False
    ]

-- props = testGroup "ListStack properties"
--   [
--     SC.testProperty "cons == push" $
--       \list x ->  push (x :: Int) (ListStack (list :: [Int])) == push (x :: Int) (ListStack (list :: [Int]))
--   ]
