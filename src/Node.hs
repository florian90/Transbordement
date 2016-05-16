module Node where

import Data.List

import Util

data Node = Node {
    n_id    :: ID,
    n_b     :: Int, -- Demand of node : <0 for depots, >0 for clients, =0 for plaateforms
    n_g     :: ValType, -- unitary cost
    n_s     :: ValType  -- transhipment time
}

emptyNode = Node 0 0 0 0

instance Indexable Node where
    idx = n_id

instance Show Node where
    show Node{n_id=_id, n_b=b, n_g=g, n_s=s} =
        "Node_" ++ show _id ++ " =\t["
        ++ "b:" ++ show b
        ++ ", g:" ++ show g
        ++ ", s:" ++ show s ++ "]"

instance Read Node where
    readsPrec _ str
        | "NODE" `isPrefixOf` str = [(Node{n_id=str#1, n_b=str#4, n_g=str#5, n_s=str#6}, "")]
        | otherwise = error $ "Node reading : " ++ str
