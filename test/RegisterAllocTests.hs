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
  [ Let (Native TyInt) "a" (Const TyInt 5),
    Let (Native TyInt) "b" (Const TyInt 30),
    Let (Native TyInt) "c" (Var (Native TyInt) "a"),
    Let (Native TyInt) "b" (Const TyInt 10),
    Let (Native TyInt) "b" (BinOp Add (Var (Native TyInt) "b") (Var (Native TyInt) "c"))
  ]

enrichedEx1 :: [EnrichedStmt]
enrichedEx1 =
  [ EnrichedStmt (StmtMetadata (fromList ["a"])) (Let (Native TyInt) "a" (Const TyInt 5)),
    EnrichedStmt (StmtMetadata (fromList ["a"])) (Let (Native TyInt) "b" (Const TyInt 30)),
    EnrichedStmt (StmtMetadata (fromList ["c"])) (Let (Native TyInt) "c" (Var (Native TyInt) "a")),
    EnrichedStmt (StmtMetadata (fromList ["b", "c"])) (Let (Native TyInt) "b" (Const TyInt 10)),
    EnrichedStmt (StmtMetadata (fromList [])) (Let (Native TyInt) "b" (BinOp Add (Var (Native TyInt) "b") (Var (Native TyInt) "c")))
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
