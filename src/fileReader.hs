module FileReader where

import System.IO
import Control.Monad

import qualified Data.Map as Map
import Data.Char
import Data.List

import Data.String
--import Data.Char
--import Data.String

import Structures

{-
data Problem = Problem {
    name :: String,
    nbNode :: Int,
    nbEdge :: Int,
    time :: Int,
    b :: [Int],     -- Demand of node : <0 for depots, >0 for clients, =0 for plaateforms
    g :: [Int],     -- unitary cost
    s :: [Int],     -- transhipment time

    u :: [[Int]],   -- capacity
    c :: [[Int]],   -- fixed cost
    h :: [[Int]],   -- unitary cost
    t :: [[Int]]    -- delivring time
} deriving (Show)
-}

getPb :: IO Problem
getPb = do
    file <- openFile "../data/transshipment1.txt" ReadMode
    pb <- initProblem file
    return pb

getPb0 :: IO Problem
getPb0 = do
    file <- openFile "../data/transshipment0.txt" ReadMode
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
                then setNbNode pb $ getElem str 1
                else if "NBR_EDGES" `isPrefixOf` str
                    then setNbEdge pb (getElem str 1)
                    else if "NODE" `isPrefixOf` str
                        then addNode pb ((getElem str 1)) (getElem str 4) (getElem str 5) (getElem str 6)
                        else if "EDGE" `isPrefixOf` str
                            then addEdge pb ((getElem str 1)) ((getElem str 2)) ((getElem str 3)) (getElem str 4) (getElem str 5) (getElem str 6) (getElem str 7)
                            else if "T" `isPrefixOf` str
                                then setTime pb (getElem str 1)
                                else pb

getElem :: String -> Int -> Int
getElem str idx = read ((words str)!!idx) :: Int

setName :: Problem -> String -> Problem
setName pb p_name = pb {pb_name=p_name}

setNbNode :: Problem -> Int -> Problem
setNbNode pb p_nbNode = pb {pb_nbNode=p_nbNode}

setNbEdge :: Problem -> Int -> Problem
setNbEdge pb p_nbEdge = pb {pb_nbEdge=p_nbEdge}

setTime :: Problem -> Int -> Problem
setTime pb p_time = pb {pb_maxTime=p_time}

addNode :: Problem -> Int -> Int -> Int -> Int -> Problem
addNode pb p_id b g s = pb{pb_nodes = (Map.insert p_id (Node p_id b g s) (pb_nodes pb) ) }

addEdge :: Problem -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Problem
addEdge pb p_id start end u c h t = pb{pb_edges = (Map.insert p_id (Edge p_id start end u c h t 0) (pb_edges pb) )}
