module Util where

import qualified Data.Map as Map

class Indexed a where
    idx :: (Indexed a) => a -> ID

type ID = Int
type ValType = Double
type Path = (ID, ID, Int)

(#) :: (Read a) => String -> Int -> a
(#) str idx
    | idx == -1 = read . last . words $ str
    | otherwise = read $ (words str)!!idx;

showMap :: (Show v) => Map.Map k v -> String
showMap m = concat . map (\x -> show x ++ "\n") $ Map.elems m
