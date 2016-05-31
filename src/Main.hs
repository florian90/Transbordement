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

{-
    Reads the parameters and execute the actions
-}
argumentLineParser :: [String] -> IO ()
argumentLineParser [] = printHelp
argumentLineParser l = argumentLineParser' l where
    argumentLineParser' [] = return ()
    argumentLineParser' (x:xs)
        | x `elem` ["-s", "--solve"] && not (null xs) = do
            solveCommand 0 (head xs)
            argumentLineParser' (tail xs)
        | x `elem` ["-ts", "--time-solve"] && (length xs >= 2)= do
            solveCommand (Time.secondsToDiffTime $ read (xs!!0)) (xs!!1)
            argumentLineParser' (tail (tail xs))
        | otherwise = printHelp

{-
    Solve the problem contained in the file given in parameter
    Show the evaluation of the solution and the time taken to solve it
    If maxTime <= 0 the time to solve the problem is the standard time
-}
solveCommand maxTime fileName = do
    pb <- getPb fileName
    putStrLn $ "Solving the problem named: " ++ pb_name pb
    let solvingTime = if maxTime <= 0 then getSolvingTime pb else maxTime
    time <- Time.getCurrentTime >>= return . Time.utctDayTime
    sol <- improveSolution pb (time + solvingTime) 0
    endTime <- Time.getCurrentTime >>= return . Time.utctDayTime
    if hasBestSolution sol
        then putStrLn $ "Tab assignmenet (" ++ showCost sol ++ ") : "++ (show $ evaluate sol)
        else putStrLn $ "No best solution found"
    if endTime - time > solvingTime
        then putStrLn $ "Evaluation stopped after " ++ show solvingTime
        else putStrLn $ "Within " ++ show (endTime - time)
    putStrLn ""
    return ()

{-
    Show how to use the program
-}
printHelp = do
    putStrLn ""
    putStrLn "Usage of the programm : ./pgname [options]"
    putStrLn "Where options are: "

    putStrLn "\t-s, --solve filename: "
    putStrLn "\t\tSolve the problem stored in the file \'filename\' with the standard time"

    putStrLn "\t-ts, --time-solve time filename: "
    putStrLn "\t\tSolve the problem stored in the file \'filename\' in less than `time` seconds"

{-
    Get the time to solve the problem
    Depend of the size (number of nodes) of the problem
-}
getSolvingTime :: Problem -> Time.DiffTime
getSolvingTime pb
    | pb_nbNode pb <= 10 = 10
    | pb_nbNode pb <= 20 = 30
    | otherwise          = 60
