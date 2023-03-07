{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}

import Control.Monad

newtype State s a = State {
    runState :: s -> (a , s)
}

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) = State $ \s ->
        let (x, s') = g s
        in (f x, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State $ \s -> (x, s)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    State h <*> State g = State $ \s ->
        let (f, s')   = h s
            (x', s'') = g s'
        in (f x', s'')

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    State h >>= f = State $ \s ->
        let (x, s') = h s
            State g = f x
        in g s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> (() , s)

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
                    (concat $ map (showAdjList) (reverse $ adjList graph))
        where
            showNewLine ls = concat $ (map (("\t" ++).(++ "\n").show) ls)
            showAdjList l = "\t" ++ show (fst l) ++ ": " ++ show (snd l) ++ "\n"

-- Adjacency List representation

allAdj :: [Edge] -> Vertex -> [Vertex]
allAdj edgeList v  = aux edgeList []
    where
        aux [] res = res
        aux (l:ls) res 
         |(getU l) == v = aux ls ((getV l):res)
         |(getV l) == v = aux ls ((getU l):res)
         |otherwise = aux ls res
        
--adjacency Representation
updateAdjList :: State Graph ()
updateAdjList  = State $ \graph -> ((), 
    Graph 
        (vertexList graph) 
        (edgeList graph) 
        [(u, allAdj (edgeList graph) u)| u <- (vertexList graph)]
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
addVerticesFoldM vs = foldM (\_ v -> addVertex v) () vs

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
addEdgesFoldM es = foldM (\_ e -> (addEdge e)) () es

removeEdge:: Edge -> State Graph ()
removeEdge edge = State $ \graph -> (() , Graph (vertexList graph) (filter (\x->x  /= edge) (edgeList graph)) [])

removeVertex :: Vertex -> State Graph () 
removeVertex vertex = State $ \graph -> (() , Graph (filter (\x -> x /= vertex) (vertexList graph)) (filter (\edge -> not (getU edge == vertex || getV edge == vertex)) (edgeList graph)) [])

dfs :: Vertex -> State Graph [Vertex]
dfs u = State $ \graph -> (reverse $ dfsTraverse (adjList graph) [] u , graph)

dfsTraverse :: AdjList -> [Vertex] -> Vertex -> [Vertex]
dfsTraverse adjListGraph visited u
    | (u `elem` visited) = visited
    | otherwise =
        let uAdjList = filter (\p -> (fst p) == u) adjListGraph
            vs = concat $ map (\pair -> snd pair) uAdjList
        in foldl (dfsTraverse adjListGraph) (u:visited) vs

v1 = Vertex "a" 
v2 = Vertex "b" 
v3 = Vertex "c" 
v4 = Vertex "d" 
v5 = Vertex "e"
v6 = Vertex "f"

e12 = Edge v1 v2
e13 = Edge v1 v3
e21 = Edge v2 v1
e34 = Edge v3 v4
e45 = Edge v4 v5

testVertex = [Vertex (show v) | v <- [1,2..6]]
testEdge = [Edge (Vertex u) (Vertex v) | (u,v) <- [
    ("1","3") ,
    ("1","2") ,
    ("2","3") ,
    ("4","5") ]]
testEdge2 = [Edge (Vertex $ show u) (Vertex $ show v) | u <- [1,2] , v <- [3,4,5,6]]

-- graphManip :: State Graph ()
graphManip = do
    addVerticesFoldM testVertex
    addEdgesFoldM testEdge2
    updateAdjList
    dfs (Vertex "5")
    -- addVertex v1

a = runState graphManip (Graph [] [] [])
