module Util where

import qualified Data.Map as Map

-- Identify an element (edge of node)
type ID = Int

-- Store the values that could be integer or float
type ValType = Double

-- Store a path, which is a movement using two nodes
--   and carry a certain amount of ressources
type Path = (ID, ID, Int)

{-
    class Indexed (quite similar to java interfaces)
      idx : Return the index of the element.
        The idx of the element must be unique (per type to be indexed)
-}
class Indexed a where
    idx :: (Indexed a) => a -> ID

{-
    Cut a string into words and returns the idx'th word,
      Return the last word if idx is -1
-}
(#) :: (Read a) => String -> Int -> a
(#) str idx
    | idx == -1 = read . last . words $ str
    | otherwise = read $ (words str)!!idx;

showMap :: (Show v) => Map.Map k v -> String
showMap m = concat . map (\x -> show x ++ "\n") $ Map.elems m
