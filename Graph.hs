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

-- instance Show a => Show (AdjGraph a) where
--     show g = show 

-- Graph
-- Vertex and edge list representation
type Graph a = ([Vertex a] , [Edge a])
-- Vertex and it's adjacent vertex
type AdjGraph a = [(Vertex a ,[Vertex a])]

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

addEdge ::Eq a => Graph a -> Edge a -> Graph a
addEdge graph edge = 
     if containsEdge graph edge
        then
            graph
        else
            (fst graph , edge : snd graph)

addEdges :: Eq a => Graph a -> [Edge a] -> Graph a
addEdges = foldl addEdge

removeVertex ::Eq a => Graph a -> Vertex a -> Graph a 
removeVertex graph v = (filter (\x -> x /= v) (gv graph) ,(ge (removeEdgebyVertex graph v) ))

removeEdge::Eq a => Graph a -> Edge a -> Graph a
removeEdge graph edge = fmap (filter (\x->x  /= edge)) graph

removeEdgebyVertex :: Eq a => Graph a -> Vertex a -> Graph a
removeEdgebyVertex graph v = fmap (filter (\edge -> not (getU edge == v|| getV edge == v))) graph

allAdj ::Eq a => [Edge a] -> Vertex a -> [Vertex a]
allAdj li v  = aux li []
    where
        aux [] res = res
        aux (l:ls) res 
         |(getU l) == v = aux ls ((getV l):res)
         |(getV l) == v = aux ls ((getU l):res)
         |otherwise = aux ls res
        

--adjacency-list Representation
adjRep ::Eq a => Graph a -> AdjGraph a
adjRep graph  = [(u,allAdj (ge graph) u)| u <- (gv graph)]


--get vertices
gv :: Graph a -> [Vertex a]
gv = fst

-- getting edgy
ge :: Graph a -> [Edge a]
ge = snd


-- dfs :: Graph a -> Vertex a-> [a]
-- dfs graph v = 

-- Graph Properties
    -- showAdjacentVertices(Vertex a)
    -- isConnected(Vertex a , Vertex b)

-- Graph => [(Vertex , [Vertex])]

v1 = Vertex "a" 3
v2 = Vertex "b" 69
v3 = Vertex "c" 6969
v4 = Vertex "d" (-69)
v5 = Vertex "e" 2

e12 = Edge v1 v2
e13 = Edge v1 v3
e21 = Edge v2 v1
e34 = Edge v3 v4
e45 = Edge v4 v5


g :: Graph Int
g = addVertices ([],[]) [v1,v2,v3,v4,v5]
g1 = addEdges g [e12,e13,e21,e34,e45] 





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

