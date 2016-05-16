module Util where

type ID = Int
type ValType = Double
type Path = (ID, ID, Int)

class Indexable a where
    idx :: a -> ID

{-
    Get a word in a String
      If the index is -1 return the last word,
      otherwise, returns the word of index idx
-}
(#) :: (Read a) => String -> Int -> a
(#) str idx
    | idx == -1 = read . last . words $ str
    | otherwise = read $ (words str)!!idx

{-
    Add an element at the right index of a list so that :
      idx (l!!x) == x
    The list must already contains enough elements
      to add the new element at the right position
-}
(§) :: (Indexable a) => [a] -> a -> [a]
(§) list el = replace (idx el) el list

replace n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replace (n-1) newVal xs
