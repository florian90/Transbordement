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
    t :: [[Int]]	-- delivring time
} deriving (Show)

main :: IO ()
main = do
    file <- openFile "../data/transshipment1.txt" ReadMode
    pb <- initProblem file
    putStrLn $ show $ pb

newProblem :: Problem
newProblem = Problem "" 0 0 0 [] [] [] [[]] [[]] [[]] [[]]

initProblem :: Handle => IO Problem
initProblem file = do
    pb <- lineReader file newProblem
    return pb

lineReader :: Handle => Problem => IO Problem
lineReader file pb = do
    l <- hGetLine file
    putStrLn l
    if "EOF" `isPrefixOf` l
        then return pb
        else lineReader file (updateProblem l pb)

updateProblem :: String => Problem => Problem
updateProblem str pb =
    if "#" `isPrefixOf` str
        then (pb {time=(pb time)})
        else if "NAME" `isPrefixOf` str
            then setName pb (last $ words $ str)
            else if "NBR_NODES" `isPrefixOf` str
                then setNbNode pb (getElem str 1)
                else if "NBR_EDGES" `isPrefixOf` str
                    then setNbEdge pb (getElem str 1)
                    else if "NODE" `isPrefixOf` str
                        then addNode pb (getElem str 1) (getElem str 3) (getElem str 4) (getElem str 5)
                        else if "EDGE" `isPrefixOf` str
                            then addEdge pb (getElem str 1) (getElem str 2) (getElem str 3) (getElem str 4) (getElem str 5) (getElem str 6) (getElem str 7)
                            else if "T" `isPrefixOf` str
                                then setTime pb (getElem str 1)
                                else pb {time=(pb time)}

getElem :: String => Int => Int
getElem str idx = read ((words str)!!idx) :: Int

setName :: Problem => String => Problem
setName pb p_name = pb {name=p_name}

setNbNode :: Problem => Int => Problem
setNbNode pb p_nbNode = pb {nbNode=p_nbNode}

setNbEdge :: Problem => Int => Problem
setNbEdge pb p_nbEdge = pb {nbEdge=p_nbEdge}

setTime :: Problem => Int => Problem
setTime pb p_time = pb {time=p_time}

--addNode :: Problem => Int => Int => Int => Int => Problem
--addNode pb idx p_b p_g p_s = pb {b=(pb b!!idx) g=(pb g!!idx) s=(pb s!!idx)}
addNode pb idx p_b p_g p_s = pb {time=(pb time)}

--addEdge :: Problem => Int => Int => Int => Int => Int => Int => Problem
addEdge pb start end u c h t = pb {time=(pb time)}

setVal :: [Int] => Int => Int => [Int]
setVal (_:xs) idx newVal
    | idx == 0 = newVal:xs
    | otherwise = setVal xs (idx-1) newVal

setVal2d :: [[Int]] => Int => Int => Int => [Int]
setVal2d (_:xs) x y newVal
    | x == 0 = (setVal x newVal)
    | otherwise = setVal2d xs (x-1) y newVal

intiTab :: Int => [Int]
intiTab i
    | i == 0 = [0]
    | otherwise = 0:(intiTab (i-1))

{-
setB :: Problem => String => Problem
setName pb name = pb { name=name}

setG :: Problem => String => Problem
setName pb name = pb { name=name}

setS :: Problem => String => Problem
setName pb name = pb { name=name}


setU :: Problem => String => Problem
setName pb name = pb { name=name}

setC :: Problem => String => Problem
setName pb name = pb { name=name}

setH :: Problem => String => Problem
setName pb name = pb { name=name}

setT :: Problem => String => Problem
setName pb name = pb { name=name}
-}
