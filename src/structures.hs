module Structures where

import qualified Data.Map as Map

data Node = Node {
    n_id    :: Int,
    n_b     :: Int, -- Demand of node : <0 for depots, >0 for clients, =0 for plaateforms
    n_g     :: Int, -- unitary cost
    n_s     :: Int  -- transhipment time
} deriving (Read)

instance Show Node where
    show Node{n_id=_id, n_b=b, n_g=g, n_s=s} =
        "Node_" ++ show _id ++ " =\t["
        ++ "b:" ++ show b
        ++ ", g:" ++ show g
        ++ ", s:" ++ show s ++ "]"

data Edge = Edge {
    e_id    :: Int,
    e_start :: Int,
    e_end   :: Int,
    e_u     :: Int, -- capacity
    e_c     :: Int, -- fixed cost
    e_h     :: Int, -- unitary cost
    e_t     :: Int, -- delivring time
    e_a     :: Int  -- actual used capacity
} deriving (Read)

instance Show Edge where
    show Edge{e_id=_id, e_start=start, e_end=end, e_u=u, e_c=c, e_h=h, e_t=t, e_a=a} =
        "Edge_" ++ show _id ++ " =\t["
        ++ "(" ++ show start ++ "->" ++ show end ++ ")"
        ++ ", u:" ++ show a ++ "/" ++ show u
        ++ ", c:" ++ show c
        ++ ", h:" ++ show h
        ++ ", t:" ++ show t ++ "]"

data Problem = Problem {
    pb_name     :: String,
    pb_nbNode   :: Int,
    pb_nbEdge   :: Int,
    pb_maxTime  :: Int,
    pb_nodes    :: Map.Map Int Node,
    pb_edges    :: Map.Map Int Edge,
    pb_solution :: Map.Map Int Int,
    pb_bestSolution :: Map.Map Int Int,
    pb_bestSolutionCost :: Int,
    pb_path :: [Path]
} deriving (Read)

newProblem = Problem "" 0 0 0 Map.empty Map.empty Map.empty Map.empty 0 []

instance Show Problem where
    show pb = pb_name pb ++ " : \n"
        ++ showMap (pb_nodes pb)
        ++ showMap (pb_edges pb)

showEverything :: Problem -> String
showEverything pb = pb_name pb ++ " : \n"
    ++ showMap (pb_nodes pb)
    ++ showMap (pb_edges pb)
    ++ "path used :\t" ++ (show . pb_path) pb ++ "\n"
    ++ (if Map.null $ pb_solution pb
            then ""
            else "Solution actuelle = " ++ show (Map.assocs (pb_solution pb)) ++ "\n")
    ++ (if Map.null $ pb_bestSolution pb
            then ""
            else "Meilleure solution (cout=" ++ (show . pb_bestSolutionCost) pb ++ ")= "
                ++ show (Map.assocs (pb_bestSolution pb)))

showSolution :: Problem -> String
showSolution pb = pb_name pb ++ " : \n"
    ++ if Map.null $ pb_bestSolution pb
            then "No best solution founds"
            else "Meilleure solution (cout=" ++ (show . pb_bestSolutionCost) pb ++ ")= "
                ++ show (Map.assocs (pb_bestSolution pb))

showMap :: (Show v) => Map.Map k v -> String
showMap m = concat . map (\x -> show x ++ "\n") $ Map.elems m

{-
    Return the edge of intex idx of the problem
-}
getEdge :: Problem -> Int -> Edge
getEdge Problem{pb_edges=edges} idx = edges Map.! idx

{-
    Return the node of intex idx of the problem
-}
getNode :: Problem -> Int -> Node
getNode Problem{pb_nodes=nodes} idx = nodes Map.! idx

type Path = Maybe (Int, Int, Int)
