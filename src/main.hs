module Main where

import Data.List

import System.Environment

import qualified Data.Time as Time

import Problem
import Solver

main :: IO ()
main = do
    args <- getArgs
    argumentLineParser args
    return ()

argumentLineParser :: [String] -> IO ()
argumentLineParser [] = printHelp
argumentLineParser l = argumentLineParser' l

argumentLineParser' :: [String] -> IO ()
argumentLineParser' [] = return ()
argumentLineParser' (x:xs)
    | x `elem` ["-s", "--solve"] && not (null xs) = do
        solveProblem (head xs)
        argumentLineParser' (tail xs)
    | otherwise = printHelp

solveProblem :: String -> IO ()
solveProblem fileName = do
    putStrLn ""
    putStrLn $ "Reading the file " ++ fileName ++ "..."
    pb <- getPb fileName
    putStrLn $ "Solving the problem named: " ++ pb_name pb
    time <- Time.getCurrentTime >>= return . Time.utctDayTime
    sol <- improveSolution pb (time + getSolvingTime pb) 0
    endTime <- Time.getCurrentTime >>= return . Time.utctDayTime
    putStrLn $ "Tab assignmenet : " ++ (show $ evaluate $ sol)
    putStrLn $ "Within " ++ show (endTime - time)
    putStrLn ""
    return ()

{-
    Get the time to solve the problem
        Depend of the size (number of nodes) of the problem
-}
getSolvingTime :: Problem -> Time.DiffTime
getSolvingTime pb
    | pb_nbNode pb <= 10 = 10
    | pb_nbNode pb <= 20 = 30
    | otherwise          = 60

printHelp = do
    putStrLn ""
    putStrLn "Usage of the programm : ./pgname [options]"
    putStrLn "Where options are: "
    putStrLn "\t-s, --solve filename: "
    putStrLn "\t\tSolve the problem stored in the file \'../data/filename\'"
