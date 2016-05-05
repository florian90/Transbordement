module Structures where

import qualified Data.Map as Map

type ID = Int

type ValType = Float

data Node = Node {
    n_id    :: ID,
    n_b     :: Int, -- Demand of node : <0 for depots, >0 for clients, =0 for plaateforms
    n_g     :: ValType, -- unitary cost
    n_s     :: ValType  -- transhipment time
}

instance Show Node where
    show Node{n_id=_id, n_b=b, n_g=g, n_s=s} =
        "Node_" ++ show _id ++ " =\t["
        ++ "b:" ++ show b
        ++ ", g:" ++ show g
        ++ ", s:" ++ show s ++ "]"

data Edge = Edge {
    e_id    :: ID,
    e_start :: ID,
    e_end   :: ID,
    e_u     :: Int, -- capacity
    e_c     :: ValType, -- fixed cost
    e_h     :: ValType, -- unitary cost
    e_t     :: ValType, -- delivring time
    e_a     :: Int  -- actual used capacity
}

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
    pb_maxTime  :: ValType,
    pb_nodes    :: Map.Map ID Node,
    pb_edges    :: Map.Map ID Edge,
    pb_solution :: Map.Map ID Int,
    pb_bestSolution :: Map.Map ID Int,
    pb_bestSolutionCost :: ValType,
    pb_path :: [Path],
    pb_remove :: [Path]
}

newProblem = Problem "" 0 0 0 Map.empty Map.empty Map.empty Map.empty 0 [] []

instance Show Problem where
    show pb = pb_name pb ++ " : \n"
        ++ showMap (pb_nodes pb)
        ++ showMap (pb_edges pb)

showResult :: Problem -> String
showResult pb = pb_name pb ++ " : \n"
    ++ showMap (pb_nodes pb)
    ++ (foldl (\acc x -> acc ++ x) [] . map showEdgeUsage $ Map.elems $ pb_edges pb)
    ++ (if Map.null $ pb_bestSolution pb
            then ""
            else "Meilleure solution (cout=" ++ (show . pb_bestSolutionCost) pb ++ ")= "
                ++ show (Map.assocs (pb_bestSolution pb)))
        where
            showEdgeUsage Edge{e_id=_id, e_start=start, e_end=end, e_u=u, e_c=c, e_h=h, e_t=t, e_a=a} =
                "Edge_" ++ show _id ++ " =\t["
                ++ "(" ++ show start ++ "->" ++ show end ++ ")"
                ++ ", u:" ++ show (if _id `Map.member` (pb_bestSolution pb) then pb_bestSolution pb Map.! _id else 0) ++ "/" ++ show u
                ++ ", c:" ++ show c
                ++ ", h:" ++ show h
                ++ ", t:" ++ show t ++ "]"
                ++ "\n"

showSolution :: Problem -> String
showSolution pb = if Map.null $ pb_bestSolution pb
            then "No best solution founds"
            else "Meilleure solution (cout=" ++ (show . pb_bestSolutionCost) pb ++ ")= "
                ++ show (Map.assocs (pb_bestSolution pb))

showMap :: (Show v) => Map.Map k v -> String
showMap m = concat . map (\x -> show x ++ "\n") $ Map.elems m

{-
    Return the edge of intex idx of the problem
-}
getEdge :: Problem -> ID -> Edge
getEdge Problem{pb_edges=edges} idx = edges Map.! idx

{-
    Return the node of intex idx of the problem
-}
getNode :: Problem -> ID -> Node
getNode Problem{pb_nodes=nodes} idx = nodes Map.! idx

type Path = (ID, ID, Int)
