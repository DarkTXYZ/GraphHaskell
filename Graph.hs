{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}

-- Graphs data
-- Vertex
data Vertex a = Vertex {
    label :: String ,
    value :: a
}

instance Show a => Show (Vertex a) where
    show u = "\"" ++ label u ++ "\""

instance Eq a => Eq (Vertex a) where
    u == v = label u == label v

-- Edge
data Edge a = Edge {
    getU :: Vertex a ,
    getV :: Vertex a
}

instance Show a => Show (Edge a) where
    -- show :: Show a => Vertex a -> [Char]
    show edge = show (getU edge) ++ " -- " ++ show (getV edge)

instance Eq a => Eq (Edge a) where
    m == n = (getU m == getU n && getV m == getV n) || (getU m == getV n && getV m == getU n)

-- Graph
-- Vertex and edge list representation
type Graph a = ([Vertex a] , [Edge a])

-- Graph Construction

containsVertex :: Eq a => Graph a -> Vertex a -> Bool
containsVertex graph u = u `elem` gv graph

containsEdge:: Eq a => Graph a -> Edge a -> Bool
containsEdge graph u = u `elem` ge graph

addVertex :: Eq a => Graph a -> Vertex a -> Graph a
addVertex graph u = 
    if containsVertex graph u 
        then
            graph
        else
            (u : fst graph , snd graph)

addVertices :: Eq a => Graph a -> [Vertex a] -> Graph a
addVertices = foldl addVertex

addEdge :: Graph a -> Edge a -> Graph a
addEdge graph edge = (fst graph , edge : snd graph)

addEdges :: Graph a -> [Edge a] -> Graph a
addEdges = foldl addEdge

gv :: Graph a -> [Vertex a]
gv = fst

ge :: Graph a -> [Edge a]
ge = snd

-- Graph Properties
    -- showAdjacentVertices(Vertex a)
    -- isConnected(Vertex a , Vertex b)

-- Graph => [(Vertex , [Vertex])]

v1 = Vertex "a" 3
v2 = Vertex "b" 69
v3 = Vertex "c" 6969
v4 = Vertex "d" (-69)
v5 = Vertex "e" 2
v6 = Vertex "pp" 002
v7 = Vertex "f" 1
v8 = Vertex "g" 99
v9 = Vertex "1xbet" 77769420
v10 = Vertex "ggwp" 1150
g :: Graph Int
g = addVertices ([],[]) [v1,v2,v3,v4,v5,v6,v7,v8,v9,v10]

-- gsss :: Graph Int
-- gsss = addEdge gss (Edge v1 v2)

-- Graph
    -- Undirected Graph
        -- Graph Construction
        -- Display Graph
        -- Get Vertex , Edges
        -- Contains Vertex , Edges

-- To Do List
    -- Transform graph to adj list !!
    -- Algorithm in unweighted graph    
        -- DFS , CC
    -- Continuation (Weighted , unweighted) ??
    -- Algorithm 
        -- MST
        -- ShortestPath
        -- Topological Sort

    