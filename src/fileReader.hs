module Main where

import System.IO
import Control.Monad

import Data.Char
import Data.List
import Data.String

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

main :: IO ()
main = do
    file <- openFile "../data/transshipment1.txt" ReadMode
    pb <- initProblem file
    putStrLn $ show $ pb

newProblem :: Problem
newProblem = Problem "" 0 0 0 [] [] [] [[]] [[]] [[]] [[]]

initProblem :: Handle -> IO Problem
initProblem file = do
    pb <- lineReader file newProblem
    return pb

lineReader :: Handle -> Problem -> IO Problem
lineReader file pb = do
    l <- hGetLine file
    putStrLn l
    if "EOF" `isPrefixOf` l
        then return pb
        else lineReader file (updateProblem l pb)

updateProblem :: String -> Problem -> Problem
updateProblem str pb =
    if "#" `isPrefixOf` str
        then pb {time= time pb}
        else if "NAME" `isPrefixOf` str
            then setName pb (last $ words $ str)
            else if "NBR_NODES" `isPrefixOf` str
                then setNbNode pb $ getElem str 1
                else if "NBR_EDGES" `isPrefixOf` str
                    then setNbEdge pb (getElem str 1)
                    else if "NODE" `isPrefixOf` str
                        then addNode pb ((getElem str 1)-1) (getElem str 4) (getElem str 5) (getElem str 6)
                        else if "EDGE" `isPrefixOf` str
                            then addEdge pb ((getElem str 2)-1) ((getElem str 3)-1) (getElem str 4) (getElem str 5) (getElem str 6) (getElem str 7)
                            else if "T" `isPrefixOf` str
                                then setTime pb (getElem str 1)
                                else pb {time=(time pb)}

getElem :: String -> Int -> Int
getElem str idx = read ((words str)!!idx) :: Int

setName :: Problem -> String -> Problem
setName pb p_name = pb {name=p_name}

setNbNode :: Problem -> Int -> Problem
setNbNode pb p_nbNode = pb {nbNode=p_nbNode,
                            b = newTab p_nbNode,
                            g = newTab p_nbNode,
                            s = newTab p_nbNode,
                            u = newTab2d p_nbNode p_nbNode,
                            c = newTab2d p_nbNode p_nbNode,
                            h = newTab2d p_nbNode p_nbNode,
                            t = newTab2d p_nbNode p_nbNode}

setNbEdge :: Problem -> Int -> Problem
setNbEdge pb p_nbEdge = pb {nbEdge=p_nbEdge}

setTime :: Problem -> Int -> Problem
setTime pb p_time = pb {time=p_time}

newTab :: Int -> [Int]
newTab idx
    | idx < 0 = error "Negative number !"
    | idx == 0 = []
    | otherwise = 0: newTab (idx-1)

newTab2d :: Int -> Int -> [[Int]]
newTab2d idx idy
    | idx < 0 = error "Negative number !"
    | idx == 0 = []
    | otherwise = (newTab idy): newTab2d (idx-1) idy

addNode :: Problem -> Int -> Int -> Int -> Int -> Problem
addNode pb idx p_b p_g p_s = pb{b = setVal (b pb) idx p_b,
                                g = setVal (g pb) idx p_g,
                                s = setVal (s pb) idx p_s}

addEdge :: Problem -> Int -> Int -> Int -> Int -> Int -> Int -> Problem
addEdge pb start end p_u p_c p_h p_t = pb { u = setVal2d (u pb) start end p_u,
                                            c = setVal2d (c pb) start end p_c,
                                            h = setVal2d (h pb) start end p_h,
                                            t = setVal2d (c pb) start end p_c}

setVal :: [Int] -> Int -> Int -> [Int]
setVal (x:xs) idx newVal
    | idx < 0 = error "Negative value"
    | idx == 0 = newVal:xs
    | otherwise = x:(setVal xs (idx-1) newVal)

setVal2d :: [[Int]] -> Int -> Int -> Int -> [[Int]]
setVal2d (x:xs) idx idy newVal
    | idx < 0 = error "Negative value"
    | idx == 0 = (setVal x idy newVal):xs
    | otherwise = x:setVal2d xs (idx-1) idy newVal

intiTab :: Int -> [Int]
intiTab i
    | i == 0 = [0]
    | otherwise = 0:(intiTab (i-1))

{-
setB :: Problem -> String -> Problem
setName pb name = pb { name=name}

setG :: Problem -> String -> Problem
setName pb name = pb { name=name}

setS :: Problem -> String -> Problem
setName pb name = pb { name=name}


setU :: Problem -> String -> Problem
setName pb name = pb { name=name}

setC :: Problem -> String -> Problem
setName pb name = pb { name=name}

setH :: Problem -> String -> Problem
setName pb name = pb { name=name}

setT :: Problem -> String -> Problem
setName pb name = pb { name=name}
-}
