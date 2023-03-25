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

updateAdj :: UndirectedGraph -> Graph
updateAdj (UDG graph) =
            Graph
                (vertexList graph)
                (edgeList graph)
                [(u, allAdj (edgeList graph) u) | u <- vertexList graph]

allAdj :: [Edge] -> Vertex -> [(Vertex , Integer)]
allAdj edgeList v = aux edgeList []
  where
    aux [] res = res
    aux (l : ls) res
        | getU l == v = aux ls ((getV l , getW l): res)
        | getV l == v = aux ls ((getU l , getW l): res)
        | otherwise = aux ls res

containsVertex :: Vertex -> State UndirectedGraph Bool
containsVertex u = State $ \(UDG graph) -> (Graph.containsVertex u graph, UDG graph)

addVertex :: Vertex -> State UndirectedGraph ()
addVertex newVertex = State $ \(UDG graph) -> (() , UDG $ Graph.addVertex newVertex graph)

addVertices :: [Vertex] -> State UndirectedGraph ()
addVertices vs = State $ \(UDG graph) -> (() , UDG $ Graph.addVertices vs graph)

removeVertex :: Vertex -> State UndirectedGraph ()
removeVertex vertex = State $ \(UDG graph) -> (() , UDG $ Graph.removeVertex vertex graph)

containsEdge :: Edge -> State UndirectedGraph Bool
containsEdge e = State $ \(UDG graph) -> (Graph.containsEdge e graph UDE, UDG graph)

addEdge :: Edge -> State UndirectedGraph ()
addEdge newEdge = State $ \(UDG graph) -> (() , UDG $ Graph.addEdge newEdge graph UDE)

addEdges :: [Edge] -> State UndirectedGraph ()
addEdges es = State $ \(UDG graph) -> ((), UDG $ Graph.addEdges es graph UDE)

removeEdge :: Edge -> State UndirectedGraph ()
removeEdge edge = State $ \(UDG graph) -> ((), UDG $ Graph.removeEdge edge graph UDE)
