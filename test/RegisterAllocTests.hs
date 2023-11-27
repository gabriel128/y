module RegisterAllocTests
  ( test_reg_alloc
  ) where

import           Ast.Ast
import           Data.Set
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           GeneralDS.Graph                ( Graph )
import           Passes.RegisterAlloc
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( assertBool
                                                , assertEqual
                                                , testCase
                                                )

test_reg_alloc :: TestTree
test_reg_alloc = testGroup "Tests" [unitTests]

-- We save the livness after
ex1 :: [Stmt]
ex1 =
  [ Let "a" (Const 5)
  , -- {a}
    Let "b" (Const 30)
  , -- {a}
    Let "c" (Var "a")
  , -- {c}
    Let "b" (Const 10)
  , -- {a, c}
    Let "b" (BinOp Add (Var "a") (Var "c")) -- {}
  ]

enrichedEx1 :: [EnrichedStmt]
enrichedEx1 =
  [ EnrichedStmt (fromList ["a"])      (Let "a" (Const 5))
  , EnrichedStmt (fromList ["a"])      (Let "b" (Const 30))
  , EnrichedStmt (fromList ["a", "c"]) (Let "c" (Var "a"))
  , EnrichedStmt (fromList ["a", "c"]) (Let "b" (Const 10))
  , EnrichedStmt (fromList [])         (Let "b" (BinOp Add (Var "a") (Var "c")))
  ]

-- interferenceGraphEx1 :: Graph Text
-- interferenceGraphEx1 =
--   let nodeA = Node "a" (Set.fromList ["b", "c"])
--       nodeB = Node "b" (Set.fromList ["c"])
--       nodeC = Node "c" (Set.fromList ["a"])
--   in  Graph (fromList [("a", nodeA), ("b", nodeB), ("c", nodeC)])

ex2 :: [Stmt]
ex2 =
  [ Let "a" (Const 5)
  , --  {a}
    Let "b" (Var "a")
  , -- {a}
    Let "c" (Var "a")
  , -- {a}
    Let "d" (Const 10)
  , -- {a}
    Return (Var "a") -- {}
  ]

livenessAfterEx2 :: [Set T.Text]
livenessAfterEx2 =
  [fromList ["a"], fromList ["a"], fromList ["a"], fromList ["a"], fromList []]

unitTests :: TestTree
unitTests = testGroup
  "Register Allocation tests"
  [ testCase "liveness doesn't modify order of stmts" $ do
    let enrichedStmts = buildLiveness ex1
    assertEqual "" (fmap stmt enrichedEx1) (fmap stmt enrichedStmts)
  , testCase "liveness_ex1" $ do
    let enrichedStmts = buildLiveness ex1
    assertEqual "" enrichedEx1 enrichedStmts
  , testCase "liveness_ex2" $ do
    let enrichedStmts = buildLiveness ex2
        liveness      = fmap livenessAfter enrichedStmts
    assertEqual "" livenessAfterEx2 liveness
  , testCase "builds interferece graph for ex1" $ do
    assertEqual "" True False
  ]
