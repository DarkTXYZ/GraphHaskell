{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Graph where

import Control.Monad
import State


-- Vertex
data Vertex = Vertex {
    label :: String
}

instance Show Vertex where
    show = label

instance Eq Vertex where
    u == v = label u == label v

-- Edge
data Edge = Edge {
    getU :: Vertex ,
    getV :: Vertex ,
    getW :: Integer
}

instance Show Edge where
    show edge = show (getU edge) ++ " <-- " ++ show (getW edge) ++ " --> " ++ show (getV edge)

instance Eq Edge where
    m == n =
        getU m == getU n && getV m == getV n ||
        getU m == getV n && getV m == getU n

type AdjList = [(Vertex ,[Vertex])]

-- Graph
-- Vertex and Edge List representation
data Graph = Graph {
    vertexList :: [Vertex] ,
    edgeList :: [Edge] ,
    adjList :: AdjList
}

instance Show Graph where
    show graph =    "\nVertices : " ++ show (reverse $ vertexList graph) ++ "\n" ++
                    "Edges : \n" ++ showNewLine (reverse $ edgeList graph) ++
                    "AdjList : \n" ++
                    concatMap showAdjList (reverse $ adjList graph)
        where
            showNewLine = concatMap (("\t" ++).(++ "\n").show)
            showAdjList l = "\t" ++ show (fst l) ++ ": " ++ show (snd l) ++ "\n"

-- Adjacency List representation

allAdj :: [Edge] -> Vertex -> [Vertex]
allAdj edgeList v  = aux edgeList []
    where
        aux [] res = res
        aux (l:ls) res
         |getU l == v = aux ls (getV l:res)
         |getV l == v = aux ls (getU l:res)
         |otherwise = aux ls res

--adjacency Representation
updateAdjList :: State Graph ()
updateAdjList  = State $ \graph -> ((),
    Graph
        (vertexList graph)
        (edgeList graph)
        [(u, allAdj (edgeList graph) u)| u <- vertexList graph]
    )

-- Graph Construction
containsVertex :: Vertex -> State Graph Bool
containsVertex u = State $ \graph -> (u `elem` vertexList graph , graph)

containsEdge:: Edge -> State Graph Bool
containsEdge e = State $ \graph -> (e `elem` edgeList graph , graph)

addVertex :: Vertex -> State Graph ()
addVertex newVertex = State $ \graph -> runState manip graph
    where
        manip = do
            contain <- containsVertex newVertex
            if not contain
                then
                    State $ \graph -> (() , Graph (newVertex : vertexList graph) (edgeList graph) [])
                else
                    State $ \graph -> (() , graph)

addVertices :: [Vertex] -> State Graph ()
addVertices [] = State $ \graph -> (() , graph)
addVertices (v:vs) = State $ \graph -> runState manip graph
    where
        manip = do
            addVertex v
            addVertices vs

-- (State Graph () -> Vertex -> State Graph ()) -> State Graph () -> [Vertex] -> State Graph ()
addVerticesFold :: [Vertex] -> State Graph ()
addVerticesFold vs = State $ \graph -> foldl (\g v -> runState (addVertex v) (snd g)) (() , graph) vs

addVerticesFoldM :: [Vertex] -> State Graph ()
addVerticesFoldM = foldM (\_ v -> addVertex v) ()

addEdge :: Edge -> State Graph ()
addEdge newEdge = State $ \graph -> runState manip graph
    where
        manip = do
            containEdge <- containsEdge newEdge
            containU <- containsVertex $ getU newEdge
            containV <- containsVertex $ getV newEdge
            if not containEdge && containU && containV
                then
                    State $ \graph -> (() , Graph (vertexList graph) (newEdge : edgeList graph) [])
                else
                    State $ \graph -> (() , graph)

addEdges :: [Edge] -> State Graph ()
addEdges [] = State $ \graph -> (() , graph)
addEdges (e:es) = State $ \graph -> runState manip graph
    where
        manip = do
            addEdge e
            addEdges es

addEdgesFold :: [Edge] -> State Graph ()
addEdgesFold es = State $ \graph -> foldl (\g e -> runState (addEdge e) (snd g)) (() , graph) es

addEdgesFoldM :: [Edge] -> State Graph ()
addEdgesFoldM = foldM (\_ e -> addEdge e) ()

removeEdge:: Edge -> State Graph ()
removeEdge edge = State $ \graph -> (() , Graph (vertexList graph) (filter (/= edge) (edgeList graph)) [])

removeVertex :: Vertex -> State Graph ()
removeVertex vertex = State $ \graph -> (() , Graph (filter (/= vertex) (vertexList graph)) (filter (\edge -> not (getU edge == vertex || getV edge == vertex)) (edgeList graph)) [])

dfs :: Vertex -> State Graph [Vertex]
dfs u = State $ \graph -> (reverse $ dfsTraverse (adjList graph) [] u , graph)

dfsTraverse :: AdjList -> [Vertex] -> Vertex -> [Vertex]
dfsTraverse adjListGraph visited u
    | u `elem` visited = visited
    | otherwise =
        let uAdjList = filter (\p -> fst p == u) adjListGraph
            vs = concatMap snd uAdjList
        in foldl (dfsTraverse adjListGraph) (u:visited) vs


dfsTraverse2 :: AdjList -> [Vertex] -> Vertex -> Vertex -> ([Vertex],Bool)
dfsTraverse2 adjListGraph visited parent u
    | u `elem` visited = (visited , False)
    | otherwise =
        let uAdjList = filter (\p -> fst p == u) adjListGraph
            vs = concatMap snd uAdjList
        in foldl (\acc v ->
            let current_visited = fst acc
                current_found = snd acc
            in
            if v == parent then
                (current_visited , current_found)
            else if v `elem` current_visited then
                (current_visited , True)
            else
                let (v_visited , v_found) = dfsTraverse2 adjListGraph current_visited u v
                in (v_visited , current_found || v_found)
        ) (u:visited , False) vs

cycleDetection:: State Graph Bool
cycleDetection = State $ \graph -> (cycleUtil graph , graph)

cycleUtil :: Graph -> Bool
cycleUtil g = snd $ foldl (
    \result vertex ->
        let visited = fst result
            foundCycle = snd result
        in
            if vertex `elem` visited then
                result
            else
                let (component , found) = dfsTraverse2 (adjList g) [] (Vertex "-1") vertex
                in
                    (visited ++ component , foundCycle || found)) ([],False) (vertexList g)


getConnectedComponents :: State Graph [[Vertex]]
getConnectedComponents = State $ \graph -> (getComponents graph , graph)

getComponents :: Graph -> [[Vertex]]
getComponents g = snd $ foldl (
    \result vertex ->
        let visited = fst result
            components = snd result
        in
            if vertex `elem` visited then
                result
            else
                let component = dfsTraverse (adjList g) [] vertex
                in
                    (visited ++ component , component : components)) ([],[]) (vertexList g)








