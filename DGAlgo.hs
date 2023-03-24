module DGAlgo where

import DG
import Graph
import State
import Data.List

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
        adjU = map fst $ concatMap snd(filter (\p -> fst p == u) adjList)

bfs :: Vertex -> State DirectedGraph [Vertex]
bfs u = State $ \(DG graph) -> (bfsTraverse (adjList graph) [u] [u], DG graph)

bfsTraverse ::  AdjList -> [Vertex] -> [Vertex] ->  [Vertex]
bfsTraverse _ visited [] =  visited
bfsTraverse adjListGraph visited (q:qs) = bfsTraverse adjListGraph (visited ++ notVisitedAdjU) newQ
    where 
        adjU = concatMap snd (filter (\p -> fst p == q) adjListGraph)
        notVisitedAdjU = map fst $ filter(\x -> fst x `notElem` visited) adjU
        newQ = qs ++ notVisitedAdjU

allInfinite:: Vertex -> Graph -> [(Vertex,Integer)]
allInfinite src graph = [(u, if(u == src) then 0 else 1000000)|  u <- vertexList graph ]

-- spt allinfinte, vertexList -> [vertex,(prev,distance if from prev)]

spt :: AdjList -> [(Vertex,Integer)] -> [(Vertex,Integer)] -> [(Vertex,Integer)] 
spt _ dist [] =  dist
spt adjListGraph dist (q:qs) = spt adjListGraph newDist newQ
    where 
        u = fst q
        vs = concatMap snd (filter (\p -> fst p == u) adjListGraph)
        (newDist, newQ) = foldl (\(cur_dist , cur_q) (vv , w) -> 
            let 
                dist_u = snd $ head $ filter ((== u).fst) dist
                dist_vv = snd $ head $ filter ((== vv).fst) dist
            in
                if (dist_vv > dist_u + w) then
                    (updateDist cur_dist vv (dist_u + w) , cur_q ++ [(vv , dist_u + w)])
                else
                    (cur_dist , cur_q)
            ) (dist , qs) vs

shortestPath :: Vertex -> State DirectedGraph [(Vertex , Integer)]
shortestPath u = State $ \(DG graph) -> (spt (adjList graph) (allInfinite u graph) [(u , 0)], DG graph)

updateDist :: [(Vertex , Integer)] -> Vertex -> Integer -> [(Vertex , Integer)]
updateDist dist v w = map (\(vertex , d) -> if (vertex == v) then (v , w) else (vertex , d)) dist

topoSort :: State DirectedGraph [Vertex]
topoSort = State $ \(DG graph) -> (topoSortUtil graph, DG graph)

topoSortUtil::Graph -> [Vertex]
topoSortUtil graph = reverse $ aux [] graph
 where 
     aux res (Graph [] _ _) = res
     aux res graph = aux (removedVertex:res) newGraph
      where
         noIncomingEdge x = foldl(\acc e -> acc && getV e /= x) True (edgeList graph) 
         removedVertex = head $ filter(\v -> noIncomingEdge v) (vertexList graph)
         newGraph = Graph.removeVertex removedVertex graph


ms :: State DirectedGraph DirectedGraph
ms = State $ \(DG graph) -> (mstUtil graph , DG graph)

mstUtil :: Graph -> DirectedGraph
mstUtil graph = DG $ foldl (
    \acc e -> 
        if DGAlgo.cycleUtil (Graph.addEdge e acc DE) 
            then acc
            else (Graph.addEdge e acc DE) ) 
        newGraph 
        (sortEdgeList)
 where
     newGraph = Graph (vertexList graph) [] []
     sortEdgeList = sortBy compare (edgeList graph)
     compare e1 e2 | getW e1 < getW e2 = LT | getW e1 == getW e2 = EQ | getW e1 > getW e2 = GT

-- sortEdgeList (Graph [] [Edge (Vertex "1") (Vertex "2") (3) , Edge (Vertex "18") (Vertex "24") (1) , Edge (Vertex "112") (Vertex "1231232") (69)] [])
