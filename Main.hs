import Graph
import State
import DG
import UDG
import UDGAlgo
import DGAlgo

testVertex = [Vertex (show v) | v <- [1, 2 .. 7]]

testEdge =
    [ Edge (Vertex u) (Vertex v) 1
    | (u, v) <-
        [ ("1", "2")
        , ("1", "4")
        , ("1", "5")
        , ("2", "3")
        , ("2", "4")
        , ("3", "5")
        , ("3", "6")
        , ("3", "7")
        , ("4", "6")
        , ("5", "2")
        , ("7", "4")
        ]
    ]

testCycleEdge = [ Edge (Vertex u) (Vertex v) 1
    | (u, v) <-
        [ ("1", "2")
        , ("1", "4")
        , ("1", "5")
        , ("2", "3")
        , ("2", "5")
        , ("3", "4")
        , ("3", "6")
        , ("4", "2")
        , ("4", "6")
        , ("5", "3")
        , ("5", "4")
        ]
    ]

testConnectedComponent = [ Edge (Vertex u) (Vertex v) 1
    | (u, v) <-
        [ ("1", "2")
        , ("2", "3")
        , ("2", "4")
        , ("3", "4")
        , ("5", "6")
        , ("6", "7")
        ]
    ]

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


testDGStpEdge =
    [ Edge (Vertex u) (Vertex v) w
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
        ]
    ]

testMstEdge =
    [ Edge (Vertex u) (Vertex v) w
    | (u, v, w) <-
        [ ("1", "2" , 9)
        , ("1", "6" , 8)
        , ("2", "6" , 2)
        , ("2", "3" , 6)
        , ("3", "4" , 3)
        , ("3", "7" , 11)
        , ("4", "5" , 10)
        , ("5", "7" , 4)
        , ("5", "6" , 1)
        , ("6", "7" , 7)
        ]
    ]

dgManip = do
    DG.addVertices testVertex

    -- DG.addEdges testEdge
    -- DG.updateAdjList
    -- DGAlgo.dfs (Vertex "1")
    -- DGAlgo.bfs (Vertex "1")

    -- DG.addEdges testCycleEdge
    -- DG.updateAdjList
    -- DGAlgo.cycleDetection

    -- DG.addEdges testDGStpEdge
    -- DG.updateAdjList
    -- DGAlgo.shortestPath (Vertex "1") 

    -- DG.addEdges testTopo
    -- DG.updateAdjList
    -- DGAlgo.topoSort

runDG = runState dgManip (DG $ Graph [] [] [])

udgManip = do
    UDG.addVertices testVertex

    -- UDG.addEdges testConnectedComponent
    -- UDG.updateAdjList
    -- UDGAlgo.getConnectedComponents

    -- UDG.addEdges testMstEdge
    -- UDG.updateAdjList
    -- UDGAlgo.ms

runUDG = runState udgManip (UDG $ Graph [] [] [])
