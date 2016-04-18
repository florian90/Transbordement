module Structures where

data Node = Node {
    n_id :: Int,
    n_b :: Int,
    n_g :: Int,
    n_s :: Int
} deriving Show

data Edge = Edge {
    e_id :: Int,
    e_start :: Int,
    e_end :: Int,
    e_u :: Int,
    e_c :: Int,
    e_h :: Int,
    e_t :: Int
} deriving Show

data Problem = Problem {
    pb_name :: String,
    pb_nbNode :: Int,
    pb_nbEdge :: Int,
    pb_maxTime :: Int,
    pb_nodes :: [Node],
    pb_edges :: [Edge]
} deriving Show
