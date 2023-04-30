{-# LANGUAGE ScopedTypeVariables #-}

module DsTests (test_general_ds, dsProps) where

import Control.Monad (join)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Debug.Trace (traceShowId)
import GeneralDS.Graph
import GeneralDS.Queue (Queue (..))
import qualified GeneralDS.Queue as Queue
import GeneralDS.Stack (Stack (..))
import qualified GeneralDS.Stack as Stack
import Test.QuickCheck.Classes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck

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
        let builtGraph = foldr insertNode newGraph nodes
        assertEqual "" graphEx builtGraph
        assertBool "" (isValidGraph builtGraph),
      testCase "builds a graph by inserting edges" $ do
        let edges = [("a", "b"), ("a", "c"), ("b", "d")]
        let builtGraph = foldr insertEdge newGraph edges
        assertEqual "" graphEx builtGraph
        assertBool "" (isValidGraph builtGraph),
      testCase "Graph is a semigroup" $ do
        let nodes =
              [ Node "a" (Set.fromList ["b", "c"]),
                Node "b" (Set.fromList ["a", "d"]),
                Node "c" (Set.fromList ["a"]),
                Node "d" (Set.fromList ["b"])
              ]
        let graph = foldr insertNode newGraph nodes
        let nodes' =
              [ Node "a" (Set.fromList ["f"]),
                Node "f" (Set.fromList ["b"])
              ]
        let graph' = foldr insertNode newGraph nodes
        let expectedNodes =
              [ Node "a" (Set.fromList ["b", "c", "f"]),
                Node "b" (Set.fromList ["a", "d", "f"]),
                Node "c" (Set.fromList ["a"]),
                Node "d" (Set.fromList ["b"]),
                Node "f" (Set.fromList ["a", "b"])
              ]
        let expectedGraph = foldr insertNode newGraph nodes
        assertEqual "" expectedGraph (graph <> graph'),
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

dsProps :: TestTree
dsProps =
  testGroup
    "DS properties"
    (graphMonoidLaws ++ queueLaws)

instance (Show a, Ord a, Arbitrary a) => Arbitrary (Node a) where
  arbitrary = do
    val <- arbitrary
    return (Node val Set.empty)

instance (Show a, Ord a, Arbitrary a) => Arbitrary (Graph a) where
  arbitrary = do
    edges :: [(a, a)] <- arbitrary
    let graph = foldr insertEdge newGraph edges
    if not . isValidGraph $ graph then error "Invalid graph" else return graph

instance (Show a, Ord a, Arbitrary a) => Arbitrary (Queue a) where
  arbitrary = do
    queueElems :: [a] <- arbitrary
    let queue = foldr Queue.enqueue Queue.new queueElems
    return queue

semigroupLaw :: (Monoid m, Eq m) => m -> Bool
semigroupLaw m = m <> mempty == m && m == mempty <> m

monoidLaw :: (Monoid m, Eq m) => m -> m -> m -> Bool
monoidLaw ma mb mc = (ma <> mb) <> mc == ma <> (mb <> mc)

functorIdLaw :: (Functor k, Eq (k a)) => k a -> Bool
functorIdLaw k = fmap id k == k

functorFusionLaw :: (Functor k, Eq (k c)) => k a -> Fun b c -> Fun a b -> Bool
functorFusionLaw k (Fun _ f) (Fun _ g) = (fmap (f . g) k) == (fmap f . fmap g $ k)

queueLaws :: [TestTree]
queueLaws =
  [ testProperty "Queue len == Elemens as list len" $
      \(queue :: Queue Int) -> fromIntegral (Queue.len queue) == length (Queue.toList queue),
    testProperty "Queue Functor laws: identity " $ \(queue :: Queue Int) -> functorIdLaw queue,
    testProperty "Queue Functor laws: fusion " $
      \(queue :: Queue Int, f :: Fun Int Int, g :: Fun Int Int) -> functorFusionLaw queue g f
  ]

graphMonoidLaws :: [TestTree]
graphMonoidLaws =
  [ testProperty "Graph Semigroup laws (identity)" $ \(graph :: Graph Int) -> semigroupLaw graph,
    testProperty "Graph Monoid law (associativity)" $ \(graph :: Graph Int, graph1 :: Graph Int, graph2 :: Graph Int) -> monoidLaw graph graph1 graph2
  ]
