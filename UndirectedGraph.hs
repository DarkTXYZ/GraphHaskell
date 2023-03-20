{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module UndirectedGraph where

import Control.Monad
import Graph
import State

newtype UndirectedEdge = UndirectedEdge Edge

instance Show UndirectedEdge where
    show (UndirectedEdge edge) = show (getU edge) ++ " <--(" ++ show (getW edge) ++ ")--> " ++ show (getV edge)

instance Eq UndirectedEdge where
    UndirectedEdge m == UndirectedEdge n =
        getU m == getU n && getV m == getV n
            || getU m == getV n && getV m == getU n

newtype UndirectedGraph = UndirectedGraph Graph

instance Show UndirectedGraph where
    show (UndirectedGraph graph) =
        "\nVertices : "
            ++ show (reverse $ vertexList graph)
            ++ "\n"
            ++ "Edges : \n"
            ++ showNewLine (reverse $ edgeList graph)
            ++ "AdjList : \n"
            ++ concatMap showAdjList (reverse $ adjList graph)
      where
        showNewLine = concatMap (("\t" ++) . (++ "\n") . show . UndirectedEdge)
        showAdjList l = "\t" ++ show (fst l) ++ ": " ++ show (snd l) ++ "\n"

updateAdjList :: State UndirectedGraph ()
updateAdjList = State $
    \(UndirectedGraph graph) ->
        ( ()
        , UndirectedGraph $
            Graph
                (vertexList graph)
                (edgeList graph)
                [(u, allAdj (edgeList graph) u) | u <- vertexList graph]
        )

containsVertex :: Vertex -> State UndirectedGraph Bool
containsVertex u = State $ \(UndirectedGraph graph) -> (u `elem` vertexList graph, UndirectedGraph graph)

addVertex :: Vertex -> State UndirectedGraph ()
addVertex newVertex = State $ \graph -> runState manip graph
  where
    manip = do
        contain <- containsVertex newVertex
        if not contain
            then State $
                \(UndirectedGraph graph) ->
                    ((), UndirectedGraph $ Graph (newVertex : vertexList graph) (edgeList graph) [])
            else State $ \graph -> ((), graph)

addVertices :: [Vertex] -> State UndirectedGraph ()
addVertices [] = State $ \graph -> ((), graph)
addVertices (v : vs) = State $ \graph -> runState manip graph
  where
    manip = do
        addVertex v
        addVertices vs

addVerticesFold :: [Vertex] -> State UndirectedGraph ()
addVerticesFold vs = State $
    \graph -> foldl (\g v -> runState (addVertex v) (snd g)) ((), graph) vs

addVerticesFoldM :: [Vertex] -> State UndirectedGraph ()
addVerticesFoldM = foldM (\_ v -> addVertex v) ()

removeVertex :: Vertex -> State UndirectedGraph ()
removeVertex vertex = State $
    \(UndirectedGraph graph) ->
        ( ()
        , UndirectedGraph $
            Graph
                (filter (/= vertex) (vertexList graph))
                ( filter
                    (\edge -> not (getU edge == vertex || getV edge == vertex))
                    (edgeList graph)
                )
                []
        )

containsEdge :: Edge -> State UndirectedGraph Bool
containsEdge e = State $ \(UndirectedGraph graph) -> (UndirectedEdge e `elem` map UndirectedEdge (edgeList graph), UndirectedGraph graph)

addEdge :: Edge -> State UndirectedGraph ()
addEdge newEdge = State $ \graph -> runState manip graph
  where
    manip = do
        containEdge <- containsEdge newEdge
        if not containEdge
            then State $ \(UndirectedGraph graph) -> ((), UndirectedGraph $ Graph (vertexList graph) (newEdge : edgeList graph) [])
            else State $ \graph -> ((), graph)

addEdges :: [Edge] -> State UndirectedGraph ()
addEdges [] = State $ \graph -> ((), graph)
addEdges (e : es) = State $ \graph -> runState manip graph
  where
    manip = do
        addEdge e
        addEdges es

addEdgesFold :: [Edge] -> State UndirectedGraph ()
addEdgesFold es = State $ \graph -> foldl (\g e -> runState (addEdge e) (snd g)) ((), graph) es

addEdgesFoldM :: [Edge] -> State UndirectedGraph ()
addEdgesFoldM = foldM (\_ e -> addEdge e) ()

removeEdge :: Edge -> State UndirectedGraph ()
removeEdge edge = State $ \(UndirectedGraph graph) -> ((), UndirectedGraph $ Graph (vertexList graph) (filter ((/= UndirectedEdge edge) . UndirectedEdge) (edgeList graph)) [])
