module Problem
(
    Problem(..),
    getNode,
    getEdge,
    getPb
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
    pb_nodes    :: [Node],
    pb_edges    :: [Edge]
}

newProblem = Problem "" 0 0 0 [] []

instance Show Problem where
    show pb = pb_name pb ++ " : \n"
        ++ "maxTime = " ++ (show . pb_maxTime $ pb) ++ "\n"
        ++ showTab (pb_nodes pb)
        ++ showTab (pb_edges pb)

{-
    Return the edge of intex idx of the problem
-}
getEdge :: Problem -> ID -> Edge
getEdge Problem{pb_edges=edges} idx = edges!!idx

{-
    Return the node of intex idx of the problem
-}
getNode :: Problem -> ID -> Node
getNode Problem{pb_nodes=nodes} idx = nodes!!idx

----------------------- Functions to get a problem from a file -----------------------

isValidProblem pb = True;

getPb :: String -> IO Problem
getPb name = do
    file <- openFile ("../data/"++name) ReadMode
    pb <- lineReader file newProblem
    return pb

lineReader :: Handle -> Problem -> IO Problem
lineReader file pb = do
    l <- hGetLine file
    if "EOF" `isPrefixOf` l
        then do
            if isValidProblem pb
            then return pb
            else error "The Problem is not correct"
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
setNbNode pb p_nbNode = pb {pb_nbNode=p_nbNode, pb_nodes=initNodes} where
    initNodes = replicate (p_nbNode+1) emptyNode

setNbEdge :: Problem -> Int -> Problem
setNbEdge pb p_nbEdge = pb {pb_nbEdge=p_nbEdge, pb_edges=initEdges}where
    initEdges = replicate (p_nbEdge+1) emptyEdge

setTime :: Problem -> ValType -> Problem
setTime pb p_time = pb {pb_maxTime=p_time}

addNode :: Problem -> Node -> Problem
addNode pb node = pb{pb_nodes = (pb_nodes pb) § node}

addEdge :: Problem -> Edge -> Problem
addEdge pb edge = pb{pb_edges = (pb_edges pb) § edge}
