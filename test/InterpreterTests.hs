module InterpreterTests (test_programs) where

import Data.Map
import Lvar
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

-- import Test.Tasty.SmallCheck as SC

test_programs :: TestTree
test_programs = testGroup "Tests" [unitTests]

expr :: Expr
expr =
  let negEight = UnaryOp Neg (Const 8)
      ast1_1 = BinOp Add (Const 42) negEight
   in ast1_1

unitTests =
  testGroup
    "ListStack"
    [ testCase "interpretExp Constant" $
        interpExpr empty (Const 8) @?= Right 8,
      --
      testCase "interpretExp Expr" $
        interpExpr empty expr @?= Right 34,
      --
      testCase "interpret correct program" $
        interpProg (Program [stmt0, stmt1, stmt2, stmt3]) @?= "2",
      --
      testCase "interpret incorrect program" $
        interpProg (Program [stmt0, stmt1, stmt3, stmt2]) @?= "Can't find variable z",
      --
      testCase "remove complex ops do nothing when not needed" $
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
