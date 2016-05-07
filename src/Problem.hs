module Problem where

import System.IO

import Control.Monad

import Data.Char
import Data.List
import Data.String

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


getPb :: String -> IO Problem
getPb name = do
    file <- openFile ("../data/"++name) ReadMode
    pb <- initProblem file
    return pb

initProblem :: Handle -> IO Problem
initProblem file = do
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
