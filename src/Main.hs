module Main where

import Data.List

import System.Environment


import Problem
import Solver

main :: IO ()
main = do
    args <- getArgs
    argumentLineParser args
    return ()

argumentLineParser :: [String] -> IO ()
argumentLineParser [] = printHelp
argumentLineParser l = argumentLineParser' l where
    argumentLineParser' :: [String] -> IO ()
    argumentLineParser' [] = return ()
    argumentLineParser' (x:xs)
        | x `elem` ["-s", "--solve"] && not (null xs) = do
            solveProblem (head xs)
            argumentLineParser' (tail xs)
        | otherwise = printHelp

{-
    Solve the problem contained in the file given in parameter
    Show the evaluation of the solution and the time taken to solve it
-}
solveProblem :: String -> IO ()
solveProblem fileName = do
    putStrLn ""
    putStrLn $ "Reading the file " ++ fileName ++ "..."
    pb <- getPb fileName
    solve pb
    return ()

{-
    Show how to use the program
-}
printHelp = do
    putStrLn ""
    putStrLn "Usage of the programm : ./pgname [options]"
    putStrLn "Where options are: "
    putStrLn "\t-s, --solve filename: "
    putStrLn "\t\tSolve the problem stored in the file \'../data/filename\'"
