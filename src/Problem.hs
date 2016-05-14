module Problem
(
    Problem(..),
    getPb,
    evaluate,
    getNode,
    getEdge,
    showSolution

)where

import System.IO

import Control.Monad

import Data.Char
import Data.List
import Data.String
import Data.Maybe

import qualified Data.Map as Map

import Util
import Node
import Edge

data Problem = Problem {
    pb_name     :: String,
    pb_nbNode   :: Int,
    pb_nbEdge   :: Int,
    pb_maxTime  :: ValType,
    pb_nodes    :: Map.Map ID Node,
    pb_edges    :: Map.Map ID Edge
}

newProblem = Problem "" 0 0 0 Map.empty Map.empty

instance Show Problem where
    show pb = pb_name pb ++ " : \n"
        ++ "maxTime = " ++ (show . pb_maxTime $ pb)
        ++ showMap (pb_nodes pb)
        ++ showMap (pb_edges pb)

data Solution = Solution {
    sol_nbEdge  :: Int,             -- From pb_nbEdge
    sol_nbNode  :: Int,             -- From pb_nbNode
    sol_nodes   :: Map.Map ID Int,  -- The remaining capacity of the nodes
    sol_edges   :: Map.Map ID Int,  -- <==> pb_solution
    sol_remove  :: [Path],          -- The edges we can't use in the actual solution

    sol_best    :: Map.Map ID Int,  -- The sol_edges of the best sol
    sol_bestCost:: Maybe ValType    -- The cost of the best solution
}

instance Show Solution where
    show sol = show . evaluate $ sol

emptySolution Problem{pb_nbEdge=e, pb_nbNode=n} = Solution e n Map.empty Map.empty [] Map.empty Nothing

-- showResult :: Problem -> String
-- showResult pb = pb_name pb ++ " : \n"
--     ++ showMap (pb_nodes pb)
--     ++ (foldl (\acc x -> acc ++ x) [] . map showEdgeUsage $ Map.elems $ pb_edges pb)
--     ++ (if Map.null $ sol_best pb
--             then ""
--             else "Meilleure solution (cout=" ++ (show . sol_bestCost) pb ++ ")= "
--                 ++ show (Map.assocs (sol_best pb)))
--         where
--             showEdgeUsage Edge{e_id=_id, e_start=start, e_end=end, e_u=u, e_c=c, e_h=h, e_t=t, e_a=a} =
--                 "Edge_" ++ show _id ++ " =\t["
--                 ++ "(" ++ show start ++ "->" ++ show end ++ ")"
--                 ++ ", u:" ++ show (if _id `Map.member` (sol_best pb) then sol_best pb Map.! _id else 0) ++ "/" ++ show u
--                 ++ ", c:" ++ show c
--                 ++ ", h:" ++ show h
--                 ++ ", t:" ++ show t ++ "]"
--                 ++ "\n"

showSolution :: Solution -> String
showSolution sol = if Map.null $ sol_best sol then "No best solution founds"
    else "Meilleure solution (cout=" ++ (show . sol_bestCost) sol ++ ")= " ++ show (Map.assocs (sol_best sol))

showMap :: (Show v) => Map.Map k v -> String
showMap m = concat . map (\x -> show x ++ "\n") $ Map.elems m

evaluate :: Solution -> [Int]
evaluate sol = map (\x -> fromMaybe 0 $ Map.lookup x (sol_best sol)) [0..sol_nbEdge sol]

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

----------------------- Functions to get a problem from a file -----------------------

getPb :: String -> IO Problem
getPb name = do
    file <- openFile ("../data/"++name) ReadMode
    pb <- lineReader file newProblem
    return pb

lineReader :: Handle -> Problem -> IO Problem
lineReader file pb = do
    l <- hGetLine file
    if "EOF" `isPrefixOf` l
        then return pb
        else lineReader file (updateProblem l pb)

updateProblem :: String -> Problem -> Problem
updateProblem str pb =
    if "#" `isPrefixOf` str
        then pb
        else if "NAME" `isPrefixOf` str
            then setName pb (last $ words $ str)
            else if "NBR_NODES" `isPrefixOf` str
                then setNbNode pb $ str#(-1)
                else if "NBR_EDGES" `isPrefixOf` str
                    then setNbEdge pb $ str#(-1)
                    else if "NODE" `isPrefixOf` str
                        then addNode pb (read str)
                        else if "EDGE" `isPrefixOf` str
                            then addEdge pb (read str)
                            else if "T" `isPrefixOf` str
                                then setTime pb ( str#(-1))
                                else error "Error during file reading"

----------------------- Setters to initialize the problem -----------------------

setName :: Problem -> String -> Problem
setName pb p_name = pb {pb_name=p_name}

setNbNode :: Problem -> Int -> Problem
setNbNode pb p_nbNode = pb {pb_nbNode=p_nbNode}

setNbEdge :: Problem -> Int -> Problem
setNbEdge pb p_nbEdge = pb {pb_nbEdge=p_nbEdge}

setTime :: Problem -> ValType -> Problem
setTime pb p_time = pb {pb_maxTime=p_time}

addNode :: Problem -> Node -> Problem
addNode pb node@Node{n_id=_id} = pb{pb_nodes = (Map.insert _id node (pb_nodes pb) ) }

addEdge :: Problem -> Edge -> Problem
addEdge pb edge@Edge{e_id=_id} = pb{pb_edges = Map.insert _id edge (pb_edges pb) }
