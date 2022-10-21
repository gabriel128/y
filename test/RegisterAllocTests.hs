module RegisterAllocTests (test_reg_alloc) where

import Ast.Ast
import Data.Set
import Passes.RegisterAlloc
import Passes.RegisterAlloc (stmt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

test_reg_alloc :: TestTree
test_reg_alloc = testGroup "Tests" [unitTests]

ex1 :: [Stmt]
ex1 =
  [ Let "a" (Const 5),
    Let "b" (Const 30),
    Let "c" (Var "a"),
    Let "b" (Const 10),
    Let "b" (BinOp Add (Var "b") (Var "c"))
  ]

enrichedEx1 :: [EnrichedStmt]
enrichedEx1 =
  [ EnrichedStmt (StmtMetadata (fromList [])) (Let "a" (Const 5)),
    EnrichedStmt (StmtMetadata (fromList ["a"])) (Let "b" (Const 30)),
    EnrichedStmt (StmtMetadata (fromList ["a"])) (Let "c" (Var "a")),
    EnrichedStmt (StmtMetadata (fromList ["c"])) (Let "b" (Const 10)),
    EnrichedStmt (StmtMetadata (fromList ["b", "c"])) (Let "b" (BinOp Add (Var "b") (Var "c")))
  ]

unitTests :: TestTree
unitTests =
  testGroup
    "Register Allocation tests"
    [ testCase "livness doesn't modify order of stmts" $ do
        let enrichedStmts = buildLiveness ex1
        assertEqual "" (fmap stmt enrichedEx1) (fmap stmt enrichedStmts),
      testCase "liveness for ex1" $ do
        let enrichedStmts = buildLiveness ex1
        assertEqual "" (fmap (liveness . stmtMetadata) enrichedEx1) (fmap (liveness . stmtMetadata) enrichedStmts)
    ]
