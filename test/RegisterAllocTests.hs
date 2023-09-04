module RegisterAllocTests (test_reg_alloc) where

import Ast.Ast
import Data.Set
import qualified Data.Text as T
import Passes.RegisterAlloc
import Passes.RegisterAlloc (stmt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

test_reg_alloc :: TestTree
test_reg_alloc = testGroup "Tests" [unitTests]

--  L(k) = (L(k+1) - W(k)) U R(k)
ex1 :: [Stmt]
ex1 =
  [ Let "a" (Const 5), -- {}
    Let "b" (Const 30), -- {a}
    Let "c" (Var "a"), -- {a}
    Let "b" (Const 10), -- {c}
    Let "b" (BinOp Add (Var "b") (Var "c")) -- {b, c}
  ]

enrichedEx1 :: [EnrichedStmt]
enrichedEx1 =
  [ EnrichedStmt (StmtMetadata (fromList [])) (Let "a" (Const 5)),
    EnrichedStmt (StmtMetadata (fromList ["a"])) (Let "b" (Const 30)),
    EnrichedStmt (StmtMetadata (fromList ["a"])) (Let "c" (Var "a")),
    EnrichedStmt (StmtMetadata (fromList ["c"])) (Let "b" (Const 10)),
    EnrichedStmt (StmtMetadata (fromList ["b", "c"])) (Let "b" (BinOp Add (Var "b") (Var "c")))
  ]

ex2 :: [Stmt]
ex2 =
  [ Let "a" (Const 5), --  {}
    Let "b" (Var "a"), -- {a}
    Let "c" (Var "a"), -- {a}
    Let "d" (Const 10), -- {a}
    Return (Var "a") -- {a}
  ]

livenessEx2 :: [Set T.Text]
livenessEx2 =
  [ fromList [],
    fromList ["a"],
    fromList ["a"],
    fromList ["a"],
    fromList ["a"]
  ]

unitTests :: TestTree
unitTests =
  testGroup
    "Register Allocation tests"
    [ testCase "livness doesn't modify order of stmts" $ do
        let enrichedStmts = buildLiveness ex1
        assertEqual "" (fmap stmt enrichedEx1) (fmap stmt enrichedStmts),
      testCase "liveness_ex1" $ do
        let enrichedStmts = buildLiveness ex1
        assertEqual "" (fmap (liveness . stmtMetadata) enrichedEx1) (fmap (liveness . stmtMetadata) enrichedStmts),
      testCase "liveness_ex2" $ do
        let enrichedStmts = buildLiveness ex2
        assertEqual "" livenessEx2 (fmap (liveness . stmtMetadata) enrichedStmts)
    ]
