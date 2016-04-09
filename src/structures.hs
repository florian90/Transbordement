
data Array2D = [[Int]]

data Problem = Problem {
    name :: String,
    nbNode :: Int,
    nbEdge :: Int,
    T :: Int,
    b :: [Int],     -- Demand of node : <0 for depots, >0 for clients, =0 for plaateforms
    g :: [Int],     -- unitary cost
    s :: [Int],     -- transhipment time

    u :: Array2D,   -- capacity
    c :: Array2D,   -- fixed cost
    h :: Array2D,   -- unitary cost
    t :: Array2D,   -- delivring time
} deriving (show)
