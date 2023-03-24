import Graph
import State
import DG
import UDG
import UDGAlgo

testVertex = [Vertex (show v) | v <- [1, 2 .. 6]]
testEdge =
    [ Edge (Vertex u) (Vertex v) 1
    | (u, v) <-
        [ ("1", "2")
        , ("1", "3")
        , ("4", "5")
        , ("4", "6")
        , ("5", "6")
        ]
    ]

testCycleEdge =
    [ Edge (Vertex u) (Vertex v) 1
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
    DG.addEdges testCycleEdge
    DG.updateAdjList
    -- getConnectedComponents
    -- dfs (Vertex "1")
    -- cycleDetection

run = runState directedManip (DG $ Graph [] [] [])

undirectedManip = do
    UDG.addVertices testVertex
    UDG.addEdges testEdge
    UDG.updateAdjList
    -- UDGAlgo.getConnectedComponents
    -- UDGAlgo.dfs (Vertex "5")
    UDGAlgo.cycleDetection

run2 = runState undirectedManip (UDG $ Graph [] [] [])
