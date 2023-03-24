import Graph
import State
import DG
import UDG
import UDGAlgo
import DGAlgo

testVertex = [Vertex (show v) | v <- [1, 2 .. 6]]
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
        [ ("1", "2") , ("2" , "3") , ("3" ,"3")]]

testCycleEdge = [ Edge (Vertex u) (Vertex v) 1
    | (u, v) <-
        [ ("1", "2")
        , ("1", "3")
        , ("4", "5")
        , ("4", "6")
        , ("5", "6")
        ]
    ]

-- testEdge2 = [Edge (Vertex $ show u) (Vertex $ show v) | u <- [1,2] , v <- [3,4,5,6]]

directedManip = do
    DG.addVertices testVertex
    DG.addEdges testEdge6969
    DG.updateAdjList
    -- getConnectedComponents
    -- dfs (Vertex "1")
    DGAlgo.cycleDetection
    -- DGAlgo.bfs (Vertex "1")

run = runState directedManip (DG $ Graph [] [] [])

undirectedManip = do
    UDG.addVertices testVertex
    UDG.addEdges testEdge
    UDG.updateAdjList
    -- UDGAlgo.getConnectedComponents
    UDGAlgo.bfs (Vertex "1")
    -- UDGAlgo.cycleDetection

run2 = runState undirectedManip (UDG $ Graph [] [] [])
