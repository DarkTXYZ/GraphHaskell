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
                [(u, allAdj (edgeList graph) u) | u <- vertexList graph]
        )

allAdj :: [Edge] -> Vertex -> [(Vertex , Int)]
allAdj edgeList v = aux edgeList []
  where
    aux [] res = res
    aux (l : ls) res
        | getU l == v = aux ls ((getV l , getW l) : res)
        | otherwise = aux ls res

containsVertex :: Vertex -> State DirectedGraph Bool
containsVertex u = State $ \(DG graph) -> (u `elem` vertexList graph, DG graph)

addVertex :: Vertex -> State DirectedGraph ()
addVertex newVertex = State $ \graph -> runState manip graph
  where
    manip = do
        contain <- containsVertex newVertex
        if not contain
            then State $
                \(DG graph) ->
                    ((), DG $ Graph (newVertex : vertexList graph) (edgeList graph) [])
            else State $ \graph -> ((), graph)

addVertices :: [Vertex] -> State DirectedGraph ()
addVertices [] = State $ \graph -> ((), graph)
addVertices (v : vs) = State $ \graph -> runState manip graph
  where
    manip = do
        addVertex v
        addVertices vs

addVerticesFold :: [Vertex] -> State DirectedGraph ()
addVerticesFold vs = State $
    \graph -> foldl (\g v -> runState (addVertex v) (snd g)) ((), graph) vs

addVerticesFoldM :: [Vertex] -> State DirectedGraph ()
addVerticesFoldM = foldM (\_ v -> addVertex v) ()

removeVertex :: Vertex -> State DirectedGraph ()
removeVertex vertex = State $
    \(DG graph) ->
        ( ()
        , DG $
            Graph
                (filter (/= vertex) (vertexList graph))
                ( filter
                    (\edge -> not (getU edge == vertex || getV edge == vertex))
                    (edgeList graph)
                )
                []
        )

containsEdge :: Edge -> State DirectedGraph Bool
containsEdge e = State $ \(DG graph) -> (DE e `elem` map DE (edgeList graph), DG graph)

addEdge :: Edge -> State DirectedGraph ()
addEdge newEdge = State $ \graph -> runState manip graph
  where
    manip = do
        containEdge <- containsEdge newEdge
        if not containEdge
            then State $ \(DG graph) -> ((), DG $ Graph (vertexList graph) (newEdge : edgeList graph) [])
            else State $ \graph -> ((), graph)

addEdges :: [Edge] -> State DirectedGraph ()
addEdges [] = State $ \graph -> ((), graph)
addEdges (e : es) = State $ \graph -> runState manip graph
  where
    manip = do
        addEdge e
        addEdges es

addEdgesFold :: [Edge] -> State DirectedGraph ()
addEdgesFold es = State $ \graph -> foldl (\g e -> runState (addEdge e) (snd g)) ((), graph) es

addEdgesFoldM :: [Edge] -> State DirectedGraph ()
addEdgesFoldM = foldM (\_ e -> addEdge e) ()

removeEdge :: Edge -> State DirectedGraph ()
removeEdge edge = State $ \(DG graph) -> ((), DG $ Graph (vertexList graph) (filter ((/= DE edge) . DE) (edgeList graph)) [])