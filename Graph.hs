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
    }

type AdjList = [(Vertex, [Vertex])]

data Graph = Graph
    { vertexList :: [Vertex]
    , edgeList :: [Edge]
    , adjList :: AdjList
    }

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

addVertex :: Vertex -> Graph
addVertex newVertex graph
    | containsVertex newVertex = Graph (newVertex : vertexList graph) (edgeList graph) (adjList graph)
    | otherwise = graph

addVertices :: [Vertex] -> State UndirectedGraph ()
addVertices [] = State $ \graph -> ((), graph)
addVertices (v : vs) = State $ \graph -> runState manip graph
  where
    manip = do
        addVertex v
        addVertices vs

addVerticesFold :: [Vertex] -> State UndirectedGraph ()
addVerticesFold vs = State $
    \graph -> foldl (\g v -> runState (addVertex v) (snd g)) ((), graph) vs

addVerticesFoldM :: [Vertex] -> State UndirectedGraph ()
addVerticesFoldM = foldM (\_ v -> addVertex v) ()

removeVertex :: Vertex -> State UndirectedGraph ()
removeVertex vertex = State $
    \(UDG graph) ->
        ( ()
        , UDG $
            Graph
                (filter (/= vertex) (vertexList graph))
                ( filter
                    (\edge -> not (getU edge == vertex || getV edge == vertex))
                    (edgeList graph)
                )
                []
        )
