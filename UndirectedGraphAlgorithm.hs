module UndirectedGraphAlgorithm where

import Graph (
    AdjList,
    Graph (adjList, vertexList),
    Vertex (Vertex),
 )
import State (State (State))
import UndirectedGraph (UndirectedGraph (..))

dfs :: Vertex -> State UndirectedGraph [Vertex]
dfs u = State $ \(UndirectedGraph graph) -> (reverse $ dfsTraverse (adjList graph) [] u, UndirectedGraph graph)

dfsTraverse :: AdjList -> [Vertex] -> Vertex -> [Vertex]
dfsTraverse adjListGraph visited u
    | u `elem` visited = visited
    | otherwise =
        let uAdjList = filter (\p -> fst p == u) adjListGraph
            vs = concatMap snd uAdjList
         in foldl (dfsTraverse adjListGraph) (u : visited) vs

dfsTraverse2 :: AdjList -> [Vertex] -> Vertex -> Vertex -> ([Vertex], Bool)
dfsTraverse2 adjListGraph visited parent u
    | u `elem` visited = (visited, False)
    | otherwise =
        let uAdjList = filter (\p -> fst p == u) adjListGraph
            vs = concatMap snd uAdjList
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
cycleDetection = State $ \(UndirectedGraph graph) -> (cycleUtil graph, UndirectedGraph graph)

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
getConnectedComponents = State $ \(UndirectedGraph graph) -> (getComponents graph, UndirectedGraph graph)

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
