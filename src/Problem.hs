module Problem (
    Problem(..),
    newProblem,
    showResult,
    showSolution,
    getPb,
    evaluate,
    getNode,
    getEdge
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
    pb_edges    :: Map.Map ID Edge,
    pb_solution :: Map.Map ID Int,
    pb_bestSolution :: Map.Map ID Int,
    pb_bestSolutionCost :: Maybe ValType,
    pb_remove :: [Path]
}

{-
    Return an empty problem
-}
newProblem = Problem "" 0 0 0 Map.empty Map.empty Map.empty Map.empty Nothing []

instance Show Problem where
    show pb = pb_name pb ++ " : \n"
        ++ showMap (pb_nodes pb)
        ++ showMap (pb_edges pb)

{-
    Detailed version to see every detail of the problem
-}
showResult :: Problem -> String
showResult pb = pb_name pb ++ " : \n"
    ++ showMap (pb_nodes pb)
    ++ (foldl (\acc x -> acc ++ x) [] . map showEdgeUsage $ Map.elems $ pb_edges pb)
    ++ (if Map.null $ pb_bestSolution pb
            then ""
            else "Meilleure solution (cout=" ++ (show . fromJust . pb_bestSolutionCost) pb ++ ")= "
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
            else "Meilleure solution (cout=" ++ (show . fromJust . pb_bestSolutionCost) pb ++ ")= "
                ++ (show . Map.assocs $ pb_bestSolution pb)

{-
    Return an array of int, representing the number of element carried by the edges
    The n'th element represents the number of prodct carried by the edge of index n
    The first element is always 0 and represents nothing
-}
evaluate :: Problem -> [Int]
evaluate pb = map (\x -> fromMaybe 0 $ Map.lookup x (pb_bestSolution pb)) [0..pb_nbEdge pb]

{-
    Return the list of problem's edges
-}
getEdges :: Problem -> [Edge]
getEdges = Map.elems . pb_edges

{-
    Return the list of problem's nodes
-}
getNodes :: Problem -> [Node]
getNodes = Map.elems . pb_nodes

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

{-
    Returns a IO Problem build according to a file
      The file must be in the directory data
    Param name : the name of the file
-}
getPb :: String -> IO Problem
getPb name = do
    file <- openFile ("../data/"++name) ReadMode
    lineReader file newProblem

{-
    Read a line from the file given in parameter
    Recursivly update the problem to build it
-}
lineReader :: Handle -> Problem -> IO Problem
lineReader file pb = do
    l <- hGetLine file
    if "EOF" `isPrefixOf` l
        then return pb
        else lineReader file (updateProblem l pb)

{-
    Update the problem with a string, which is a line from a data file
-}
updateProblem :: String -> Problem -> Problem
updateProblem str pb
    | "#" `isPrefixOf` str = pb
    | "NAME" `isPrefixOf` str       = setName pb (last $ words $ str)
    | "NBR_NODES" `isPrefixOf` str  = setNbNode pb $ str#(-1)
    | "NBR_EDGES" `isPrefixOf` str  = setNbEdge pb $ str#(-1)
    | "NODE" `isPrefixOf` str       = addNode pb (read str)
    | "EDGE" `isPrefixOf` str       = addEdge pb (read str)
    |  "T" `isPrefixOf` str         = setTime pb ( str#(-1))
    | otherwise                     = error "Unknow line during parsing the file"

-- ################ Setters to partialy build the problem #######################
{-
    Add an element to build the problem
-}

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
