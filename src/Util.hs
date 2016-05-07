module Util where

type ID = Int
type ValType = Float
type Path = (ID, ID, Int)

(#) :: (Read a) => String -> Int -> a
(#) str idx
    | idx == -1 = read . last . words $ str
    | otherwise = read $ (words str)!!idx
