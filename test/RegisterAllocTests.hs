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

-- We save the livness after
ex1 :: [Stmt]
ex1 =
  [ Let "a" (Const 5), -- {a}
    Let "b" (Const 30), -- {a}
    Let "c" (Var "a"), -- {c}
    Let "b" (Const 10), -- {b, c}
    Let "b" (BinOp Add (Var "b") (Var "c")) -- {}
  ]

enrichedEx1 :: [EnrichedStmt]
enrichedEx1 =
  [ EnrichedStmt (fromList ["a"]) (fromList ["a"]) (Let "a" (Const 5)),
    EnrichedStmt (fromList ["a"]) (fromList ["b"]) (Let "b" (Const 30)),
    EnrichedStmt (fromList ["c"]) (fromList ["c"]) (Let "c" (Var "a")),
    EnrichedStmt (fromList ["b", "c"]) empty (Let "b" (Const 10)),
    EnrichedStmt (fromList [""]) (fromList ["b"]) (Let "b" (BinOp Add (Var "b") (Var "c")))
  ]

ex2 :: [Stmt]
ex2 =
  [ Let "a" (Const 5), --  {a}
    Let "b" (Var "a"), -- {a}
    Let "c" (Var "a"), -- {a}
    Let "d" (Const 10), -- {a}
    Return (Var "a") -- {}
  ]

livenessAfterEx2 :: [(Set T.Text, Set T.Text)]
livenessAfterEx2 =
  [ (fromList ["a"], fromList ["a"]),
    (fromList ["a"], fromList ["b"]),
    (fromList ["a"], fromList ["c"]),
    (fromList ["a"], fromList ["d"]),
    (fromList [], empty)
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
        assertEqual "" enrichedEx1 enrichedStmts,
      testCase "liveness_ex2" $ do
        let enrichedStmts = buildLiveness ex2
            liveness = fmap livenessAfter enrichedStmts
            writeSet = fmap stmtWriteSet enrichedStmts
        assertEqual "" livenessAfterEx2 (zip liveness writeSet)
    ]
