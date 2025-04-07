module RegisterAllocTests (test_reg_alloc) where

import Ast.Ast
import Data.Set
import Optimizations.RegisterAlloc
import Optimizations.RegisterAlloc (stmt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Types.Defs

test_reg_alloc :: TestTree
test_reg_alloc = testGroup "Tests" [unitTests]

ex1 :: [Stmt]
ex1 =
  [ Let (Native ImmTy I64) "a" (Const I64 5),
    Let (Native ImmTy I64) "b" (Const I64 30),
    Let (Native ImmTy I64) "c" (Var (Native ImmTy I64) "a"),
    Let (Native ImmTy I64) "b" (Const I64 10),
    Let (Native ImmTy I64) "b" (BinOp Add (Var (Native ImmTy I64) "b") (Var (Native ImmTy I64) "c"))
  ]

enrichedEx1 :: [EnrichedStmt]
enrichedEx1 =
  [ EnrichedStmt (StmtMetadata (fromList ["a"])) (Let (Native ImmTy I64) "a" (Const I64 5)),
    EnrichedStmt (StmtMetadata (fromList ["a"])) (Let (Native ImmTy I64) "b" (Const I64 30)),
    EnrichedStmt (StmtMetadata (fromList ["c"])) (Let (Native ImmTy I64) "c" (Var (Native ImmTy I64) "a")),
    EnrichedStmt (StmtMetadata (fromList ["b", "c"])) (Let (Native ImmTy I64) "b" (Const I64 10)),
    EnrichedStmt (StmtMetadata (fromList [])) (Let (Native ImmTy I64) "b" (BinOp Add (Var (Native ImmTy I64) "b") (Var (Native ImmTy I64) "c")))
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
