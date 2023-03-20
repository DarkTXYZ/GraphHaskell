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

-- Adjacency List representation
allAdj :: [Edge] -> Vertex -> [Vertex]
allAdj edgeList v = aux edgeList []
  where
    aux [] res = res
    aux (l : ls) res
        | getU l == v = aux ls (getV l : res)
        | getV l == v = aux ls (getU l : res)
        | otherwise = aux ls res
