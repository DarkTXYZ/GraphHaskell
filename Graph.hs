{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Graph where

-- Vertex
data Vertex = Vertex {label :: String}

instance Show Vertex where
    show = label

instance Eq Vertex where
    u == v = label u == label v

-- Edge
data Edge = Edge
    { getU :: Vertex
    , getV :: Vertex
    , getW :: Integer
    } deriving Show

type AdjList = [(Vertex, [Vertex])]

data Graph = Graph
    { vertexList :: [Vertex]
    , edgeList :: [Edge]
    , adjList :: AdjList
    } deriving Show

displayGraph :: Show b => Graph -> (Edge -> b) -> [Char]
displayGraph graph f = 
            "\nVertices : "
            ++ show (reverse $ vertexList graph)
            ++ "\n"
            ++ "Edges : \n"
            ++ showNewLine f (reverse $ edgeList graph)
            ++ "AdjList : \n"
            ++ concatMap showAdjList (reverse $ adjList graph)
      where
        showNewLine f = concatMap (("\t" ++) . (++ "\n") . show . f)
        showAdjList l = "\t" ++ show (fst l) ++ ": " ++ show (snd l) ++ "\n"

containsVertex :: Vertex -> Graph -> Bool
containsVertex u graph = u `elem` vertexList graph

addVertex :: Vertex -> Graph -> Graph
addVertex newVertex graph
    | (containsVertex newVertex graph) = graph
    | otherwise = Graph (newVertex : vertexList graph) (edgeList graph) (adjList graph)

addVertices :: [Vertex] -> Graph -> Graph
addVertices [] graph = graph
addVertices (v : vs) graph = addVertices vs (addVertex v graph) 

-- addVerticesFold :: [Vertex] -> State UndirectedGraph ()
-- addVerticesFold vs = State $
--     \graph -> foldl (\g v -> runState (addVertex v) (snd g)) ((), graph) vs

-- addVerticesFoldM :: [Vertex] -> State UndirectedGraph ()
-- addVerticesFoldM = foldM (\_ v -> addVertex v) ()

removeVertex :: Vertex -> Graph -> Graph
removeVertex vertex graph = 
    Graph (filter (/= vertex) (vertexList graph))
        ( filter
            (\edge -> not (getU edge == vertex || getV edge == vertex))
            (edgeList graph)
        )
        (adjList graph)
    
containsEdge :: Eq a => Edge -> Graph -> (Edge -> a) -> Bool
containsEdge e graph edgeType = edgeType e `elem` map edgeType (edgeList graph)

addEdge :: Eq a => Edge -> Graph -> (Edge -> a) -> Graph
addEdge newEdge graph edgeType
        | containsEdge newEdge graph edgeType = graph 
        | otherwise = Graph (vertexList graph) (newEdge : edgeList graph) (adjList graph)

addEdges :: Eq a => [Edge] -> Graph -> (Edge -> a) -> Graph
addEdges [] graph _ = graph
addEdges (e : es) graph edgeType = addEdges es (addEdge e graph edgeType) edgeType

-- addEdgesFold :: [Edge] -> State UndirectedGraph ()
-- addEdgesFold es = State $ \graph -> foldl (\g e -> runState (addEdge e) (snd g)) ((), graph) es

-- addEdgesFoldM :: [Edge] -> State UndirectedGraph ()
-- addEdgesFoldM = foldM (\_ e -> addEdge e) ()

removeEdge :: Eq a => Edge -> Graph -> (Edge -> a) -> Graph
removeEdge edge graph edgeType = Graph (vertexList graph) (filter ((/= edgeType edge) . edgeType) (edgeList graph)) (adjList graph)
