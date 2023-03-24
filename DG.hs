{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module DG where

import Graph
import State
import Control.Monad

newtype DirectedEdge = DE Edge

instance Show DirectedEdge where
    show (DE edge) = show (getU edge) ++ " --(" ++ show (getW edge) ++ ")--> " ++ show (getV edge)

instance Eq DirectedEdge where
    DE m == DE n =
        getU m == getU n && getV m == getV n

newtype DirectedGraph = DG Graph

instance Show DirectedGraph where
    show (DG graph) = displayGraph graph DE

updateAdjList :: State DirectedGraph ()
updateAdjList = State $
    \(DG graph) ->
        ( ()
        , DG $
            Graph
                (vertexList graph)
                (edgeList graph)
                [(u, map fst $ allAdj (edgeList graph) u) | u <- vertexList graph]
        )

allAdj :: [Edge] -> Vertex -> [(Vertex , Integer)]
allAdj edgeList v = aux edgeList []
  where
    aux [] res = res
    aux (l : ls) res
        | getU l == v = aux ls ((getV l , getW l) : res)
        | otherwise = aux ls res

containsVertex :: Vertex -> State DirectedGraph Bool
containsVertex u = State $ \(DG graph) -> (Graph.containsVertex u graph, DG graph)

addVertex :: Vertex -> State DirectedGraph ()
addVertex newVertex = State $ \(DG graph) -> (() , DG $ Graph.addVertex newVertex graph)

addVertices :: [Vertex] -> State DirectedGraph ()
addVertices vs = State $ \(DG graph) -> (() , DG $ Graph.addVertices vs graph)

removeVertex :: Vertex -> State DirectedGraph ()
removeVertex vertex = State $ \(DG graph) -> (() , DG $ Graph.removeVertex vertex graph)

containsEdge :: Edge -> State DirectedGraph Bool
containsEdge e = State $ \(DG graph) -> (Graph.containsEdge e graph DE, DG graph)

addEdge :: Edge -> State DirectedGraph ()
addEdge newEdge = State $ \(DG graph) -> (() , DG $ Graph.addEdge newEdge graph DE)

addEdges :: [Edge] -> State DirectedGraph ()
addEdges es = State $ \(DG graph) -> ((), DG $ Graph.addEdges es graph DE)

-- addEdgesFold :: [Edge] -> State DirectedGraph ()
-- addEdgesFold es = State $ \graph -> foldl (\g e -> runState (addEdge e) (snd g)) ((), graph) es

-- addEdgesFoldM :: [Edge] -> State DirectedGraph ()
-- addEdgesFoldM = foldM (\_ e -> addEdge e) ()

removeEdge :: Edge -> State DirectedGraph ()
removeEdge edge = State $ \(DG graph) -> ((), DG $ Graph.removeEdge edge graph DE)
