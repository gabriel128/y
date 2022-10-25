module GeneralDSTests (test_general_ds) where

import Control.Monad (join)
import qualified Data.Set as Set
import GeneralDS.Graph
import GeneralDS.Queue (Queue (..))
import qualified GeneralDS.Queue as Queue
import GeneralDS.Stack (Stack (..))
import qualified GeneralDS.Stack as Stack
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

test_general_ds :: TestTree
test_general_ds = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Graph tests"
    [ testCase "builds a graph by inserting Nodes" $ do
        let nodes =
              [ Node "a" (Set.fromList ["b", "c"]),
                Node "b" (Set.fromList ["a", "d"]),
                Node "c" (Set.fromList ["a"]),
                Node "d" (Set.fromList ["b"])
              ]
            builtGraph = foldr insertNode newGraph nodes
         in assertEqual "" graphEx builtGraph,
      testCase "DFS" $
        do
          let nodes =
                [ Node "a" (Set.fromList ["b", "c"]),
                  Node "b" (Set.fromList ["a", "d"]),
                  Node "c" (Set.fromList ["a"]),
                  Node "d" (Set.fromList ["b"]),
                  Node "e" (Set.fromList ["f"]),
                  Node "f" (Set.fromList ["e"])
                ]
          let graph = foldr insertNode newGraph nodes
          assertEqual "" (dfs "a" graph) (Right ["a", "b", "d", "c"])
          assertEqual "" (dfs "b" graph) (Right ["b", "a", "c", "d"])
          assertEqual "" (dfs "c" graph) (Right ["c", "a", "b", "d"])
          assertEqual "" (dfs "d" graph) (Right ["d", "b", "a", "c"])
          assertEqual "" (dfs "e" graph) (Right ["e", "f"])
          assertEqual "" (dfs "f" graph) (Right ["f", "e"])
          assertEqual "" (dfs "g" graph) (Left "Source not found"),
      testCase "BFS" $
        do
          let nodes =
                [ Node "a" (Set.fromList ["b", "c"]),
                  Node "b" (Set.fromList ["a", "d"]),
                  Node "c" (Set.fromList ["a"]),
                  Node "d" (Set.fromList ["b"]),
                  Node "e" (Set.fromList ["f"]),
                  Node "f" (Set.fromList ["e"])
                ]
          let graph = foldr insertNode newGraph nodes
          assertEqual "" (bfs "a" graph) (Right [(0, "a"), (1, "b"), (1, "c"), (2, "d")])
          assertEqual "" (bfs "b" graph) (Right [(0, "b"), (1, "a"), (1, "d"), (2, "c")])
          assertEqual "" (bfs "c" graph) (Right [(0, "c"), (1, "a"), (2, "b"), (3, "d")])
          assertEqual "" (bfs "d" graph) (Right [(0, "d"), (1, "b"), (2, "a"), (3, "c")])
          assertEqual "" (bfs "e" graph) (Right [(0, "e"), (1, "f")])
          assertEqual "" (bfs "f" graph) (Right [(0, "f"), (1, "e")])
          assertEqual "" (dfs "g" graph) (Left "Source not found"),
      testCase
        "Stack tests"
        $ do
          let stack = Stack.new
          assertEqual "" stack (Stack [] :: Stack Int)
          let stack1 = Stack.push 2 stack
          assertEqual "" stack1 (Stack [2] :: Stack Int)
          let stack2 = Stack.push 3 stack1
          assertEqual "" stack2 (Stack [3, 2])
          let stack3 = Stack.pop stack2
          assertEqual "" stack3 (Just 3, Stack [2])
          let stack4 = Stack.pop $ snd stack3
          assertEqual "" stack4 (Just 2, Stack [])
          let stack5 = Stack.pop $ snd stack4
          assertEqual "" stack5 (Nothing, Stack []),
      testCase "Queue tests" $ do
        let queue = Queue.new
        assertEqual "" (Queue.toList queue) ([] :: [Int])
        let queue1 = Queue.enqueue 1 queue
        assertEqual "" (Queue.toList queue1) [1]
        let queue2 = Queue.enqueue 2 queue1
        assertEqual "" (Queue.toList queue2) [1, 2]
        let (res, queue3) = Queue.dequeue queue2
        assertEqual "" (Queue.toList queue3) [2]
        assertEqual "" res (Just 1)
        let (res1, queue4) = Queue.dequeue queue3
        assertEqual "" (Queue.toList queue4) []
        assertEqual "" res1 (Just 2)
        let (res2, queue5) = Queue.dequeue queue4
        assertEqual "" (Queue.toList queue5) []
        assertEqual "" res2 Nothing
    ]
