{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module GeneralDS.Graph where

import Data.Either.Combinators (maybeToRight)
import Data.List (foldl')
import Data.Map (fromList)
import qualified Data.Map as Map
import Data.Map.Strict (Map, mapWithKey)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GeneralDS.Queue (Queue)
import qualified GeneralDS.Queue as Queue
import GeneralDS.Stack (Stack)
import qualified GeneralDS.Stack as Stack

newtype Graph a = Graph {graphNodes :: Map a (Node a)}
  deriving (Show, Eq, Ord)

data Node a = Node {nodeVal :: !a, nodeEdges :: Set a}
  deriving (Show, Eq, Ord)

newGraph :: Graph a
newGraph = Graph Map.empty

isValidGraph :: Ord a => Graph a -> Bool
isValidGraph (Graph graphNodes) =
  let nodes = Map.elems graphNodes
      edges = foldr Set.union Set.empty . fmap nodeEdges $ nodes
      nodeVals = foldr Set.insert Set.empty . fmap nodeVal $ nodes
   in edges == nodeVals

newNode :: Ord a => a -> [a] -> Node a
newNode a edges = Node a (Set.fromList edges)

insertEdge :: Ord a => (a, a) -> Graph a -> Graph a
insertEdge (nodeIdA, nodeIdB) (Graph nodes) =
  let insertFn nodeId createdNode oldNode = Node nodeId (nodeEdges createdNode `Set.union` nodeEdges oldNode)
      nodes' = Map.insertWith (insertFn nodeIdA) nodeIdA (newNode nodeIdA [nodeIdB]) nodes
      nodes'' = Map.insertWith (insertFn nodeIdB) nodeIdB (newNode nodeIdB [nodeIdA]) nodes'
   in Graph nodes''

insertNode :: (Eq a, Ord a) => Node a -> Graph a -> Graph a
insertNode node@(Node a edges) (Graph nodes) =
  let insertEdgeToNode edge (Node val nodeEdges) = Node val (Set.insert edge nodeEdges)
      oldNodes = mapWithKey (\k node' -> if k `elem` edges then insertEdgeToNode a node' else node') nodes
   in Graph $ Map.insert a node oldNodes

{-
  a --- b -- d
   \
    --- c

      <>

  a --- f -- h -- b
   \
    --- c

    =

     -- f -- h
   /         |
  a -- b -- /
   \
    -- c

-}
-- Graph {nodes = fromList [(0,Node {nodeVal = 1, nodeEdges = fromList [0]})]}
instance (Ord a) => Semigroup (Graph a) where
  (Graph lnodes) <> (Graph rnodes) = Graph $ foldr reducer lnodes rnodes
    where
      reducer rnode@(Node nodeId edges) acc =
        let lnode' = fromMaybe rnode $ Map.lookup nodeId lnodes
            newEdges = edges `Set.union` nodeEdges lnode'
         in Map.insert nodeId (Node nodeId newEdges) acc

instance (Ord a) => Monoid (Graph a) where
  mempty = newGraph

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
    go Nothing _ _ = Left "Source not found"
    go (Just (Node nodeId edges)) visited result
      | nodeId `Set.member` visited = Right (visited, result)
      | otherwise = do
          let visited' = Set.insert nodeId visited
          foldl' reducer (Right (visited', nodeId : result)) edges

    reducer :: Either Text (Set a, [a]) -> a -> Either Text (Set a, [a])
    reducer visited nodeId = do
      node <- maybeToRight (T.pack $ "Node " <> show nodeId <> " not found") (Map.lookup nodeId nodes)
      (visited', result) <- visited
      go (Just node) visited' result

bfs :: forall a. Ord a => a -> Graph a -> Either Text [(Word, a)]
bfs source (Graph nodes) = doBfs (Map.lookup source nodes)
  where
    doBfs Nothing = Left "Source not found"
    doBfs (Just (Node nodeId _)) = go Set.empty (Queue.enqueue nodeId Queue.new) 0

    go :: Set a -> Queue a -> Word -> Either Text [(Word, a)]
    go visited queue distance
      | Queue.len queue == 0 = Right []
      | otherwise = do
          let currentNodeIds = Queue.toList queue
          let visited' = foldr Set.insert visited currentNodeIds
          currentNodes <- maybeToRight "Node not found" (mapM (`Map.lookup` nodes) currentNodeIds)
          let nodeIdsToEqueue = concatMap (Set.toList . flip Set.difference visited' . nodeEdges) currentNodes
          let nextQueue = foldl' (flip Queue.enqueue) Queue.new nodeIdsToEqueue
          let resultWithDistance = fmap (distance,) currentNodeIds
          (++) <$> Right resultWithDistance <*> go visited' nextQueue (distance + 1)

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
