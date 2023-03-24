module UDGAlgo where

import Graph
import State
import UDG
import Data.List

dfs :: Vertex -> State UndirectedGraph [Vertex]
dfs u = State $ \(UDG graph) -> (reverse $ dfsTraverse (adjList graph) [] u, UDG graph)

dfsTraverse :: AdjList -> [Vertex] -> Vertex -> [Vertex]
dfsTraverse adjListGraph visited u
    | u `elem` visited = visited
    | otherwise =
        let uAdjList = filter (\p -> fst p == u) adjListGraph
            vs = map fst $ concatMap snd uAdjList
         in foldl (dfsTraverse adjListGraph) (u : visited) vs

dfsTraverse2 :: AdjList -> [Vertex] -> Vertex -> Vertex -> ([Vertex], Bool)
dfsTraverse2 adjListGraph visited parent u
    | u `elem` visited = (visited, False)
    | otherwise =
        let uAdjList = filter (\p -> fst p == u) adjListGraph
            vs = map fst $ concatMap snd uAdjList
         in foldl
                ( \acc v ->
                    let current_visited = fst acc
                        current_found = snd acc
                     in if v == parent
                            then (current_visited, current_found)
                            else
                                if v `elem` current_visited
                                    then (current_visited, True)
                                    else
                                        let (v_visited, v_found) = dfsTraverse2 adjListGraph current_visited u v
                                         in (v_visited, current_found || v_found)
                )
                (u : visited, False)
                vs

cycleDetection :: State UndirectedGraph Bool
cycleDetection = State $ \(UDG graph) -> (cycleUtil graph, UDG graph)

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
                            let (component, found) = dfsTraverse2 (adjList g) [] (Vertex "-1") vertex
                             in (visited ++ component, foundCycle || found)
            )
            ([], False)
            (vertexList g)
            
getConnectedComponents :: State UndirectedGraph [[Vertex]]
getConnectedComponents = State $ \(UDG graph) -> (getComponents graph, UDG graph)

getComponents :: Graph -> [[Vertex]]
getComponents g =
    snd $
        foldl
            ( \result vertex ->
                let visited = fst result
                    components = snd result
                 in if vertex `elem` visited
                        then result
                        else
                            let component = dfsTraverse (adjList g) [] vertex
                             in (visited ++ component, component : components)
            )
            ([], [])
            (vertexList g)

bfs :: Vertex -> State UndirectedGraph [Vertex]
bfs u = State $ \(UDG graph) -> (bfsTraverse (adjList graph) [u] [u], UDG graph)

bfsTraverse ::  AdjList -> [Vertex] -> [Vertex] ->  [Vertex]
bfsTraverse _ visited [] =  visited
bfsTraverse adjListGraph visited (q:qs) = bfsTraverse adjListGraph (visited ++ notVisitedAdjU) newQ
    where 
        adjU = concatMap snd (filter (\p -> fst p == q) adjListGraph)
        notVisitedAdjU = map fst $ filter(\x -> fst x `notElem` visited) adjU
        newQ = qs ++ notVisitedAdjU

ms :: State UndirectedGraph UndirectedGraph
ms = State $ \(UDG graph) -> (mstUtil graph , UDG graph)

mstUtil :: Graph -> UndirectedGraph
mstUtil graph = UDG $ foldl (
    \acc e -> 
        if (UDGAlgo.cycleUtil) $ (UDG.updateAdj) $ (UDG $ (Graph.addEdge e acc UDE) )
            then acc
            else (UDG.updateAdj) $ (UDG $ (Graph.addEdge e acc UDE) ) )
        newGraph 
        (sortEdgeList)
 where
     newGraph = Graph (vertexList graph) [] []
     sortEdgeList = sortBy compare (edgeList graph)
     compare e1 e2 | getW e1 < getW e2 = LT | getW e1 == getW e2 = EQ | getW e1 > getW e2 = GT
