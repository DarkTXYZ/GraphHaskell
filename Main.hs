import Graph
import State

testVertex = [Vertex (show v) | v <- [1,2..7]]
testEdge = [Edge (Vertex u) (Vertex v) | (u,v) <- [
    ("1","2") ,
    ("1","3") ,
    ("4","5") ,
    ("4","6") ,
    ("5","6") ]]
-- testEdge2 = [Edge (Vertex $ show u) (Vertex $ show v) | u <- [1,2] , v <- [3,4,5,6]]

-- graphManip :: State Graph ()
graphManip = do
    addVertices testVertex
    addEdges testEdge
    updateAdjList
    -- getConnectedComponents
    -- dfs (Vertex "1")
    cycleDetection

run = runState graphManip (Graph [] [] [])
