{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module UDG where

import Control.Monad
import Graph
import State

newtype UndirectedEdge = UDE Edge

instance Show UndirectedEdge where
    show (UDE edge) = show (getU edge) ++ " <--(" ++ show (getW edge) ++ ")--> " ++ show (getV edge)

instance Eq UndirectedEdge where
    UDE m == UDE n =
        getU m == getU n && getV m == getV n
            || getU m == getV n && getV m == getU n

newtype UndirectedGraph = UDG Graph

instance Show UndirectedGraph where
    show (UDG graph) = displayGraph graph UDE


updateAdjList :: State UndirectedGraph ()
updateAdjList = State $
    \(UDG graph) ->
        ( ()
        , UDG $
            Graph
                (vertexList graph)
                (edgeList graph)
                [(u, allAdj (edgeList graph) u) | u <- vertexList graph]
        )

-- Adjacency List representation
allAdj :: [Edge] -> Vertex -> [Vertex]
allAdj edgeList v = aux edgeList []
  where
    aux [] res = res
    aux (l : ls) res
        | getU l == v = aux ls (getV l : res)
        | getV l == v = aux ls (getU l : res)
        | otherwise = aux ls res

containsVertex :: Vertex -> State UndirectedGraph Bool
containsVertex u = State $ \(UDG graph) -> (u `elem` vertexList graph, UDG graph)

addVertex :: Vertex -> State UndirectedGraph ()
addVertex newVertex = State $ \graph -> runState manip graph
  where
    manip = do
        contain <- containsVertex newVertex
        if not contain
            then State $
                \(UDG graph) ->
                    ((), UDG $ Graph (newVertex : vertexList graph) (edgeList graph) [])
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
    \(UDG graph) ->
        ( ()
        , UDG $
            Graph
                (filter (/= vertex) (vertexList graph))
                ( filter
                    (\edge -> not (getU edge == vertex || getV edge == vertex))
                    (edgeList graph)
                )
                []
        )

containsEdge :: Edge -> State UndirectedGraph Bool
containsEdge e = State $ \(UDG graph) -> (UDE e `elem` map UDE (edgeList graph), UDG graph)

addEdge :: Edge -> State UndirectedGraph ()
addEdge newEdge = State $ \graph -> runState manip graph
  where
    manip = do
        containEdge <- containsEdge newEdge
        if not containEdge
            then State $ \(UDG graph) -> ((), UDG $ Graph (vertexList graph) (newEdge : edgeList graph) [])
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
removeEdge edge = State $ \(UDG graph) -> ((), UDG $ Graph (vertexList graph) (filter ((/= UDE edge) . UDE) (edgeList graph)) [])
