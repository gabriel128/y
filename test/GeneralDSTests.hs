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
      testCase "Stack tests" $ do
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
