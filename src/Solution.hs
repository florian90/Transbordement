module Solution
(
    Solution(..),
    evaluate,
    getNode',
    getEdge',
    showSolution,
    getBestCost,
    emptySolution,
)where

import Data.Maybe

import qualified Data.Map as Map

import Util
import Node
import Edge
import Problem

data Solution = Solution {
    sol_nbNode  :: Int,             -- From pb_nbNode
    sol_nbEdge  :: Int,             -- From pb_nbEdge
    sol_nodes   :: Map.Map ID Int,  -- The remaining capacity of the nodes
    sol_edges   :: Map.Map ID Int,  -- The number of product that is carried on the edge ID edge
    sol_remove  :: [Path],          -- The edges we can't use in the actual solution

    sol_best    :: Map.Map ID Int,  -- The sol_edges of the best sol
    sol_bestCost:: Maybe ValType    -- The cost of the best solution
}

instance Show Solution where
    show sol = if isNothing (sol_bestCost sol) then "No solution found" else show . evaluate $ sol

getBestCost :: Solution -> ValType
getBestCost Solution{sol_bestCost=c} = fromMaybe (-1) c

emptySolution Problem{pb_nbNode=nbn, pb_nbEdge=nbe, pb_nodes=nodes, pb_edges=edges} =
    Solution nbn nbe mapNodes mapEdges [] Map.empty Nothing where
        mapNodes = foldl (\acc x -> Map.insert (idx x) (n_b x) acc) Map.empty nodes
        mapEdges = foldl (\acc x -> Map.insert (idx x) 0 acc) Map.empty edges

showSolution :: Solution -> String
showSolution sol = if Map.null $ sol_best sol then "No best solution founds"
    else "Meilleure solution (cout=" ++ (show . sol_bestCost) sol ++ ")= " ++ show (Map.assocs (sol_best sol));

evaluate :: Solution -> [Int]
evaluate sol = map (\x -> fromMaybe 0 $ Map.lookup x (sol_best sol)) [0..sol_nbEdge sol]

getEdge' :: Solution -> ID -> Int
getEdge' Solution{sol_edges=edges} idx = edges Map.! idx

getNode' :: Solution -> ID -> Int
getNode' Solution{sol_nodes=nodes} idx = nodes Map.! idx

-- showResult :: Problem -> Solution -> String
-- showResult pb sol = pb_name pb ++ " : \n"
--     ++ showMap (pb_nodes pb)
--     ++ (foldl (\acc x -> acc ++ x) [] . map showEdgeUsage $ Map.elems $ pb_edges pb)
--     ++ (if Map.null $ sol_best pb
--         then ""
--         else "Meilleure solution (cout=" ++ (show . sol_bestCost) pb ++ ")= "
--             ++ show . Map.assocs . sol_best $ pb)
--         where
--             showEdgeUsage Edge{e_id=_id, e_start=start, e_end=end, e_u=u, e_c=c, e_h=h, e_t=t, e_a=a} =
--                 "Edge_" ++ show _id ++ " =\t["
--                 ++ "(" ++ show start ++ "->" ++ show end ++ ")"
--                 ++ ", u:" ++ show (if _id `Map.member` (sol_best pb) then sol_best pb Map.! _id else 0) ++ "/" ++ show u
--                 ++ ", c:" ++ show c
--                 ++ ", h:" ++ show h
--                 ++ ", t:" ++ show t ++ "]"
--                 ++ "\n"
