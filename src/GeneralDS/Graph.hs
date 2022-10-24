{-# LANGUAGE ScopedTypeVariables #-}

module GeneralDS.Graph where

import Data.List (foldl')
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

-- | Returns a list of connected nodes in DFS order
dfs :: forall a. (Show a, Ord a) => a -> Graph a -> Either Text [a]
dfs source (Graph nodes) = reverse . snd <$> go (Map.lookup source nodes) Set.empty []
  where
    go :: Maybe (Node a) -> Set a -> [a] -> Either Text (Set a, [a])
    go Nothing _ _ = Left "Node not found"
    go (Just (Node nodeId edges)) visited result
      | Set.member nodeId visited = Right (Set.empty, [])
      | otherwise = do
        let visited' = Set.insert nodeId visited
        let newResult = nodeId : result
        let notVisitedNodes = fmap (`Map.lookup` nodes) . filter (\neighbourId -> not . Set.member neighbourId $ visited') $ Set.toList edges
        foldl' reducer (Right (visited', newResult)) notVisitedNodes

    reducer :: Either Text (Set a, [a]) -> Maybe (Node a) -> Either Text (Set a, [a])
    reducer visited node = do
      (visited', result) <- visited
      go node visited' result

-- | Returns the node given a node val/id using DFS
dfsNode :: forall a. Ord a => a -> Graph a -> Maybe (Node a)
dfsNode val (Graph nodes) = go (Map.elems nodes)
  where
    go [] = Nothing
    go (node : _) = recurr Set.empty (Stack.push (nodeVal node) Stack.new)

    recurr :: Set a -> Stack a -> Maybe (Node a)
    recurr visitedNodeIds nextNodeIds = do
      (node, newNextNodeIds) <- popNode nextNodeIds

      let nodeId = nodeVal node
      let edges = nodeEdges node

      case (nodeVal node == val, Set.member nodeId visitedNodeIds) of
        (True, _) -> Just node
        (False, True) -> recurr visitedNodeIds newNextNodeIds
        (False, False) ->
          let nextSet = Set.insert nodeId visitedNodeIds
              nextStack = foldr Stack.push newNextNodeIds edges
           in recurr nextSet nextStack

    popNode :: Stack a -> Maybe (Node a, Stack a)
    popNode nextNodeIds = do
      let (maybeNodeId, newNextNodeIds) = Stack.pop nextNodeIds
      nodeId <- maybeNodeId
      node <- Map.lookup nodeId nodes
      pure (node, newNextNodeIds)

bfs :: a -> Graph a -> Maybe (Node a)
bfs = undefined
