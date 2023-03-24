module DGAlgo where

import DG
import Graph
import State

-- dfsCyclic :: AdjList -> [Vertex] -> [Vertex] -> Vertex -> ([Vertex] , Bool)
-- dfsCyclic adjListGraph visited recStack u
--     | u `notElem` visited = (visited, True)
--     | otherwise =
--         let uAdjList = filter (\p -> fst p == u) adjListGraph
--             vs = concatMap snd uAdjList
--          in foldl
--                 ( \acc v ->
--                     let current_visited = fst $ fst acc
--                         current_recStack = snd $ fst acc
--                         current_found = snd acc
--                      in if v == parent
--                             then (current_visited, current_found)
--                             else
--                                 if v `elem` current_visited
--                                     then (current_visited, True)
--                                     else
--                                         let (v_visited, v_found) = dfsTraverse2 adjListGraph current_visited u v
--                                          in (v_visited, current_found || v_found)
--                 )
--                 ((u : visited , u : recStack), False)
--                 vs
-- cycleUtil :: Graph -> Bool
-- cycleUtil g =
--     snd $
--         foldl
--             ( \result vertex ->
--                 let visited = fst result
--                     foundCycle = snd result
--                  in if vertex `elem` visited
--                         then result
--                         else
--                             let (component, found) = dfsCyclic (adjList g) [] [] vertex
--                              in (visited ++ component, foundCycle || found)
--             )
--             ([], False)
--             (vertexList g)

-- cycleDetection :: State DirectedGraph Bool
-- cycleDetection = State $ \(DG graph) -> (cycleUtil graph, DG graph)

-- adjlist,visited,q->[order]

bfs :: Vertex -> State DirectedGraph [Vertex]
bfs u = State $ \(DG graph) -> (bfsTraverse (adjList graph) [u] [u], DG graph)

bfsTraverse ::  AdjList -> [Vertex] -> [Vertex] ->  [Vertex]
bfsTraverse _ visited [] =  visited
bfsTraverse adjListGraph visited (q:qs) = bfsTraverse adjListGraph (visited ++ notVisitedAdjU) newQ
 where 
     adjU = concatMap snd(filter (\p -> fst p == q) adjListGraph)
     notVisitedAdjU = filter(\x -> x `notElem` visited) adjU
     newQ = qs ++ notVisitedAdjU

