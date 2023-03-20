import Graph (Edge (Edge), Graph (Graph), Vertex (Vertex))
import State ( State(runState) )
import DirectedGraph
import UndirectedGraph
-- import UndirectedGraphAlgorithm (cycleDetection, dfs, getConnectedComponents)

testVertex = [Vertex (show v) | v <- [1, 2 .. 7]]
testEdge =
    [ Edge (Vertex u) (Vertex v) 1
    | (u, v) <-
        [ ("1", "2")
        , ("1", "3")
        , ("4", "5")
        , ("4", "6")
        , ("7", "6")
        ]
    ]

-- testEdge2 = [Edge (Vertex $ show u) (Vertex $ show v) | u <- [1,2] , v <- [3,4,5,6]]

directedManip = do
    DirectedGraph.addVertices testVertex
    DirectedGraph.addEdges testEdge
    DirectedGraph.updateAdjList
    -- getConnectedComponents
    -- dfs (Vertex "1")
    -- cycleDetection

run = runState directedManip (DirectedGraph $ Graph [] [] [])

undirectedManip = do
    UndirectedGraph.addVertices testVertex
    UndirectedGraph.addEdges testEdge
    UndirectedGraph.updateAdjList
    -- getConnectedComponents
    -- dfs (Vertex "1")
    -- cycleDetection

run2 = runState undirectedManip (UndirectedGraph $ Graph [] [] [])
