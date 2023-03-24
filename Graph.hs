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