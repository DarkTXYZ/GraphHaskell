module DGAlgo where

import DG
import Graph
import State

-- adjlist,visited,q->[order]

cycleDetection :: State DirectedGraph Bool
cycleDetection = State $ \(DG graph) -> (cycleUtil graph , DG graph)

cycleUtil :: Graph -> Bool
cycleUtil g = 
        snd $
        foldl
            ( \result vertex ->
                let visited = fst result
                    foundCycle = snd result
                 in if vertex `elem` visited
                        then result
                        else
                            let (component, found) = cycleTraverse (adjList g) [vertex] [vertex] vertex
                             in (visited ++ component, foundCycle || found)
            )
            ([], False)
            (vertexList g)
            

cycleTraverse ::  AdjList -> [Vertex] -> [Vertex] -> Vertex -> ([Vertex] , Bool)
cycleTraverse adjList visited stack u = 
    foldl (\acc v -> 
        if v `elem` stack then
            (fst acc , True)
        else
            if v `notElem` visited then
                let (v_visited , found) = cycleTraverse adjList visited (u:stack) v
                in (v_visited , snd acc || found)
            else
                acc
        
        ) (u : visited , False) adjU
    where
        adjU = concatMap snd(filter (\p -> fst p == u) adjList)

bfs :: Vertex -> State DirectedGraph [Vertex]
bfs u = State $ \(DG graph) -> (bfsTraverse (adjList graph) [u] [u], DG graph)

bfsTraverse ::  AdjList -> [Vertex] -> [Vertex] ->  [Vertex]
bfsTraverse _ visited [] =  visited
bfsTraverse adjListGraph visited (q:qs) = bfsTraverse adjListGraph (visited ++ notVisitedAdjU) newQ
 where 
     adjU = concatMap snd(filter (\p -> fst p == q) adjListGraph)
     notVisitedAdjU = filter(\x -> x `notElem` visited) adjU
     newQ = qs ++ notVisitedAdjU

