module RegisterAllocTests (test_reg_alloc) where

import Ast.Ast
import Data.Set
import Optimizations.RegisterAlloc
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

test_reg_alloc :: TestTree
test_reg_alloc = testGroup "Register Allocation Tests" unitTests

ex1 :: [Stmt]
ex1 =
  [ Let "a" (Const (NativeInt 5)),
    Let "b" (Const (NativeInt 30)),
    Let "c" (Var "a"),
    Let "b" (Const (NativeInt 10)),
    Let "b" (BinOp Add (Var "b") (Var "c"))
  ]

enrichedEx1 :: [EnrichedStmt]
enrichedEx1 =
  [ EnrichedStmt (StmtMetadata (fromList ["a"])) (Let "a" (Const (NativeInt 5))),
    EnrichedStmt (StmtMetadata (fromList ["a"])) (Let "b" (Const (NativeInt 30))),
    EnrichedStmt (StmtMetadata (fromList ["c"])) (Let "c" (Var "a")),
    EnrichedStmt (StmtMetadata (fromList ["b", "c"])) (Let "b" (Const (NativeInt 10))),
    EnrichedStmt (StmtMetadata (fromList [])) (Let "b" (BinOp Add (Var "b") (Var "c")))
  ]

unitTests :: [TestTree]
unitTests =
  [ testCase "livness doesn't modify order of stmts" $ do
      let enrichedStmts = buildLiveness ex1
      assertEqual "" (fmap stmt enrichedEx1) (fmap stmt enrichedStmts),
    testCase "liveness for ex1" $ do
      let enrichedStmts = buildLiveness ex1
      assertEqual "" (fmap (liveness . stmtMetadata) enrichedEx1) (fmap (liveness . stmtMetadata) enrichedStmts)
  ]
