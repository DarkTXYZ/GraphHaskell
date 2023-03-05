-- Vertex
data Vertex = Vertex {
    label :: String
}

instance Show Vertex where
    show u = label u

instance Eq Vertex where
    u == v = label u == label v

-- Edge
data Edge = Edge {
    getU :: Vertex ,
    getV :: Vertex
}

instance Show Edge where
    show edge = show (getU edge) ++ " <--> " ++ show (getV edge)

instance Eq Edge where
    m == n = 
        (getU m == getU n && getV m == getV n) || 
        (getU m == getV n && getV m == getU n)

-- Graph
-- Vertex and Edge List representation
data Graph = Graph {
    vertexList :: [Vertex] ,
    edgeList :: [Edge]
}

instance Show Graph where 
    show graph =    "Vertices : " ++ show (vertexList graph) ++ "\n" ++
                    "Edges : \n" ++
                    showNewLine (edgeList graph)
        where
            showNewLine ls = concat $ (map ((++ "\n").show) ls)

-- Adjacency List representation
type AdjGraph = [(Vertex ,[Vertex])]

-- Graph Construction
containsVertex :: Graph -> Vertex -> Bool
containsVertex graph u = u `elem` vertexList graph

containsEdge:: Graph -> Edge -> Bool
containsEdge graph u = u `elem` edgeList graph

addVertex :: Graph -> Vertex -> Graph
addVertex graph u = 
    if containsVertex graph u 
        then
            graph
        else
            Graph (u : vertexList graph) (edgeList graph)

addVertices :: Graph -> [Vertex] -> Graph
addVertices = foldl addVertex

addEdge :: Graph -> Edge -> Graph
addEdge graph edge = 
    if containsEdge graph edge
        then
            graph
        else
            Graph (vertexList graph) (edge : edgeList graph)

addEdges :: Graph -> [Edge] -> Graph
addEdges = foldl addEdge

removeEdgebyVertex :: Graph -> Vertex -> [Edge]
removeEdgebyVertex graph v = (filter notRemovedEdge) (edgeList graph)
    where notRemovedEdge = \edge -> not (getU edge == v || getV edge == v)

removeVertex :: Graph -> Vertex -> Graph 
removeVertex graph v = Graph (filter notRemovedVertex (vertexList graph)) (removeEdgebyVertex graph v)
    where 
        notRemovedVertex = \x -> x /= v

removeEdge:: Graph -> Edge -> Graph
removeEdge graph edge = Graph (vertexList graph) ((filter notRemovedEdge) (edgeList graph))
    where 
        notRemovedEdge = \x->x  /= edge

-- allAdj ::Eq a => [Edge a] -> Vertex a -> [Vertex a]
-- allAdj l v  = aux l []
--     where
--         aux [] res = res
--         aux (l:ls) res 
--          |(getU l) == v = aux ls ((getV l):res)
--          |(getV l) == v = aux ls ((getU l):res)
--          |otherwise = aux ls res
        

-- --adjacency Representation
-- adjRep ::Eq a => Graph a -> AdjGraph a
-- adjRep graph  = [(u,allAdj (edgeList graph) u)| u <- (vertexList graph)]
-- -- adjRep graph = 
-- --     [(u,[v]) | u <- (vertexList graph), 
-- --     v <- foldl (\acc edge -> if (getU edge == u|| getV edge == u) 
-- --         then if (getU edge) == u 
-- --                 then (getV edge):acc 
-- --                 else (getU edge):acc 
-- --               else acc ) (edgeList graph) ] 

v1 = Vertex "a" 
v2 = Vertex "b" 
v3 = Vertex "c" 
v4 = Vertex "d" 
v5 = Vertex "e"

e12 = Edge v1 v2
e13 = Edge v1 v3
e21 = Edge v2 v1
e34 = Edge v3 v4
e45 = Edge v4 v5

g :: Graph
g = addVertices (Graph [] []) [v1,v2,v3,v4,v5]
g1 = addEdges g [e12,e13,e21,e34,e45] 

-- Graph
    -- Graph Construction
        -- Add Vertex
        -- Add Edge
    -- Display Graph
    -- Remove Vertex , Edge
    -- Transform to Adjacency List representation

-- To Do List
    -- Algorithm in unweighted graph    
        -- DFS , CC , Count Components
    -- Continuation (Weighted , unweighted) ??
    -- Algorithm 
        -- MST
        -- ShortestPath
        -- Topological Sort