module GeneralDS.Graph where

import Data.Map (fromList)
import qualified Data.Map as Map
import Data.Map.Strict (Map, mapWithKey)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GeneralDS.Stack (Stack)
import qualified GeneralDS.Stack as Stack

newtype Graph a = Graph {nodes :: Map a (Node a)}
  deriving (Show, Eq)

data Node a = Node {nodeVal :: !a, nodeEdges :: Set a}
  deriving (Show, Eq)

newGraph :: Graph a
newGraph = Graph Map.empty

insertEdge :: Ord a => a -> Node a -> Node a
insertEdge edge (Node val edges) = Node val (Set.insert edge edges)

insertNode :: (Eq a, Ord a) => Node a -> Graph a -> Graph a
insertNode node@(Node a edges) (Graph nodes) =
  let oldNodes = mapWithKey (\k node' -> if k `elem` edges then insertEdge a node' else node') nodes
   in Graph $ Map.insert a node oldNodes

{-
  a --- b -- d
   \
    --- c
-}
graphEx :: Graph Text
graphEx =
  let nodeA = Node "a" (Set.fromList ["b", "c"])
      nodeB = Node "b" (Set.fromList ["a", "d"])
      nodeC = Node "c" (Set.fromList ["a"])
      nodeD = Node "d" (Set.fromList ["b"])
   in Graph (fromList [("a", nodeA), ("b", nodeB), ("c", nodeC), ("d", nodeD)])

-- | Finds node with depth first search. Starts from an arbitrary node
dfs :: (Eq a, Ord a) => a -> Graph a -> Maybe (Node a)
dfs valToFind (Graph nodes) =
  case Map.elems nodes of
    [] -> Nothing
    (node : _) -> go (Set.fromList [nodeVal node]) (Stack.push node Stack.new)
  where
    go :: Set a -> Stack (Node a) -> Maybe (Node a)
    go visitedNodes nextNodes = undefined

bfs :: a -> Graph a -> Maybe (Node a)
bfs = undefined
