module Edge where

import Data.List

import Util
import Node

data Edge = Edge {
    e_id    :: ID,
    e_start :: ID,
    e_end   :: ID,
    e_u     :: Int, -- capacity
    e_c     :: ValType, -- fixed cost
    e_h     :: ValType, -- unitary cost
    e_t     :: ValType, -- delivring time
    e_a     :: Int  -- actual usage
}

{-
    Return the remaining capacity of the edge
-}
e_r :: Edge -> Int
e_r Edge{e_u=u, e_a=a} = u - a

instance Show Edge where
    show Edge{e_id=_id, e_start=start, e_end=end, e_u=u, e_c=c, e_h=h, e_t=t, e_a=a} =
        "Edge_" ++ show _id ++ " =\t["
        ++ "(" ++ show start ++ "->" ++ show end ++ ")"
        ++ ", u:" ++ show a ++ "/" ++ show u
        ++ ", c:" ++ show c
        ++ ", h:" ++ show h
        ++ ", t:" ++ show t ++ "]"

instance Read Edge where
    readsPrec _ str
        | "EDGE" `isPrefixOf` str && (length . words $ str) >= 6 = [(Edge{e_id=str#1, e_start=str#2, e_end=str#3, e_u=str#4, e_c=str#5, e_h=str#6, e_t=str#7, e_a=0}, "")]
        | otherwise = error $ "Edge reading : " ++ str
