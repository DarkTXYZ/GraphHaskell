import Graph
import State
import DG
import UDG
import UDGAlgo
import DGAlgo

testVertex = [Vertex (show v) | v <- [1, 2 .. 7]]
testTopo = [ Edge (Vertex u) (Vertex v) 1
    | (u, v) <-
        [ ("1", "2")
        , ("1", "6")
        , ("1", "7")
        , ("2", "3")
        , ("2", "7")
        , ("3", "7")
        , ("4", "3")
        , ("4", "5")
        , ("4", "7")
        , ("6", "5")
        , ("7", "6")
        ]
    ]

testEdge =
    [ Edge (Vertex u) (Vertex v) 1
    | (u, v) <-
        [ ("1", "2")
        , ("1", "4")
        , ("2", "3")
        , ("3", "5")
        , ("4", "2")
        , ("4", "6")
        -- , ("3" , "4")
        ]
    ]
testEdge69 =  [ Edge (Vertex u) (Vertex v) 1
    | (u, v) <-
        [ ("1", "2")
        , ("2", "3")
        , ("3", "4")
        -- , ("4", "1")
        ]
    ]

testEdge6969 =  [ Edge (Vertex u) (Vertex v) 1
    | (u, v) <-
        [ ("1", "2") , ("2" , "3")]]

testCycleEdge = [ Edge (Vertex u) (Vertex v) 1
    | (u, v) <-
        [ ("1", "2")
        , ("1", "3")
        , ("4", "5")
        , ("4", "6")
        , ("5", "6")
        ]
    ]

testStpEdge =
    [ Edge (Vertex u) (Vertex v) (w)
    | (u, v, w) <-
        [ ("1", "2" , 3)
        , ("1", "4" , 6)
        , ("2", "4" , 1)
        , ("2", "3" , 10)
        , ("3", "5" , 9)
        , ("4", "2" , 5)
        , ("4", "3" , 8)
        , ("4", "5" , 4)
        , ("5", "3" , 2)
        -- , ("3" , "4")
        ]
    ]

-- testEdge2 = [Edge (Vertex $ show u) (Vertex $ show v) | u <- [1,2] , v <- [3,4,5,6]]

directedManip = do
    DG.addVertices testVertex
    DG.addEdges testTopo
    DG.updateAdjList
    DGAlgo.topoSort
    -- DGAlgo.shortestPath (Vertex "1") 
    -- getConnectedComponents
    -- dfs (Vertex "1")
    -- DGAlgo.cycleDetection
    -- DGAlgo.bfs (Vertex "1")

run = runState directedManip (DG $ Graph [] [] [])

undirectedManip = do
    UDG.addVertices testVertex
    UDG.addEdges testEdge
    UDG.updateAdjList
    -- UDGAlgo.getConnectedComponents
    -- UDGAlgo.bfs (Vertex "1")
    -- UDGAlgo.cycleDetection

run2 = runState undirectedManip (UDG $ Graph [] [] [])
