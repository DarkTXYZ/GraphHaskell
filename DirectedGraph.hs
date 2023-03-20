{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module DirectedGraph where

import Graph
import State
import Control.Monad

newtype DirectedEdge = DirectedEdge Edge

instance Show DirectedEdge where
    show (DirectedEdge edge) = show (getU edge) ++ " --(" ++ show (getW edge) ++ ")--> " ++ show (getV edge)

instance Eq DirectedEdge where
    DirectedEdge m == DirectedEdge n =
        getU m == getU n && getV m == getV n

newtype DirectedGraph = DirectedGraph Graph

instance Show DirectedGraph where
    show (DirectedGraph graph) =
        "\nVertices : "
            ++ show (reverse $ vertexList graph)
            ++ "\n"
            ++ "Edges : \n"
            ++ showNewLine (reverse $ edgeList graph)
            ++ "AdjList : \n"
            ++ concatMap showAdjList (reverse $ adjList graph)
      where
        showNewLine = concatMap (("\t" ++) . (++ "\n") . show . DirectedEdge)
        showAdjList l = "\t" ++ show (fst l) ++ ": " ++ show (snd l) ++ "\n"

updateAdjList :: State DirectedGraph ()
updateAdjList = State $
    \(DirectedGraph graph) ->
        ( ()
        , DirectedGraph $
            Graph
                (vertexList graph)
                (edgeList graph)
                [(u, allAdj (edgeList graph) u) | u <- vertexList graph]
        )

containsVertex :: Vertex -> State DirectedGraph Bool
containsVertex u = State $ \(DirectedGraph graph) -> (u `elem` vertexList graph, DirectedGraph graph)

addVertex :: Vertex -> State DirectedGraph ()
addVertex newVertex = State $ \graph -> runState manip graph
  where
    manip = do
        contain <- containsVertex newVertex
        if not contain
            then State $
                \(DirectedGraph graph) ->
                    ((), DirectedGraph $ Graph (newVertex : vertexList graph) (edgeList graph) [])
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
    \(DirectedGraph graph) ->
        ( ()
        , DirectedGraph $
            Graph
                (filter (/= vertex) (vertexList graph))
                ( filter
                    (\edge -> not (getU edge == vertex || getV edge == vertex))
                    (edgeList graph)
                )
                []
        )

containsEdge :: Edge -> State DirectedGraph Bool
containsEdge e = State $ \(DirectedGraph graph) -> (DirectedEdge e `elem` map DirectedEdge (edgeList graph), DirectedGraph graph)

addEdge :: Edge -> State DirectedGraph ()
addEdge newEdge = State $ \graph -> runState manip graph
  where
    manip = do
        containEdge <- containsEdge newEdge
        if not containEdge
            then State $ \(DirectedGraph graph) -> ((), DirectedGraph $ Graph (vertexList graph) (newEdge : edgeList graph) [])
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
removeEdge edge = State $ \(DirectedGraph graph) -> ((), DirectedGraph $ Graph (vertexList graph) (filter ((/= DirectedEdge edge) . DirectedEdge) (edgeList graph)) [])