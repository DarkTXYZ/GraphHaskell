-- Graphs data
    -- Vertex
data Vertex a = Vertex {
    label :: String ,
    value :: a
} deriving Show
    -- Edge
type Edge a = (Vertex a , Vertex a)
    -- Graph
type Graph a = (([Vertex a] , [Edge a]) , [(Vertex a , [Vertex a])] )



-- Graph Construction
    -- Add Edge --> Graph a -> Edge a -> Graph a

-- addVertex :: Graph a -> Vertex a -> Graph a
-- addVertex graph u = fst fst graph

-- getVertices :: Graph a -> [Vertex a]
-- -- getVertices [] = []
-- -- getVertices (g : gs) = fst g : getVertices gs 

-- -- containEdge :: Graph a -> Edge a -> Bool
-- containsVertex :: Graph a -> Vertex a -> Bool
-- containsVertex graph u = u `elem` getVertices graph

-- addEdge :: Graph a -> Edge a -> Graph a
-- addEdge graph (u , v) = 


-- Graph Properties
    -- Vertices
    -- Edges
    -- showAdjacentVertices(Vertex a)
    -- isConnected(Vertex a , Vertex b)


-- Graph => [(Vertex , [Vertex])]