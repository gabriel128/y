module InterpreterTests (test_programs) where

import Ast.Ast
import Ast.Examples
import Data.Map
import Interpreter.Eval
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Types.Defs

-- import Test.Tasty.SmallCheck as SC

test_programs :: TestTree
test_programs = testGroup "Tests" [unitTests]

expr :: Expr
expr =
  let negEight = UnaryOp Neg (Const I64 (NativeInt 8))
      ast1_1 = BinOp Add (Const I64 (NativeInt 42)) negEight
   in ast1_1

unitTests :: TestTree
unitTests =
  testGroup
    "ListStack"
    [ testCase "interpretExp Constant" $
        interpExpr empty (Const I64 (NativeInt 8)) @?= Right (NativeInt 8),
      --
      testCase "interpretExp Expr" $
        interpExpr empty expr @?= Right (NativeInt 34),
      --
      testCase "interpret correct program" $
        interpProg (Program [stmt0, stmt1, stmt2, stmt3]) @?= "NativeInt 2"
        --
        -- testCase "interpret incorrect program" $
        --   interpProg (Program [stmt0, stmt1, stmt3, stmt2]) @?= "Can't find variable z"
    ]

-- props = testGroup "ListStack properties"
--   [
--     SC.testProperty "cons == push" $
--       \list x ->  push (x :: Int) (ListStack (list :: [Int])) == push (x :: Int) (ListStack (list :: [Int]))
--   ]
