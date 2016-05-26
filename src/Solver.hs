module Solver where

import Prelude hiding (lookup, minBound)

import Data.List (sortBy, minimumBy)

import System.Environment

import Data.Bool
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Time as Time

import Util
import Node
import Edge
import Problem

solve = improveSolution

improveSolution :: Problem -> Time.DiffTime -> Int -> IO Problem
improveSolution pb endTime i = do
    time <- Time.getCurrentTime >>= return . Time.utctDayTime
    if endTime < time
        then do
            -- Time is over
            return pb
        else if not . isFeasible $ pb
            then
                -- We can't find a solution with the given constraints -> cut
                return pb
            else if isSolutionOver pb
                -- New solution for the problem !
                -- If it's better than the actual best solution, save it, otherwise return the old problem
                then if isBestSolution pb
                    then newBestSolution pb
                    else do
                        return pb
                else if hasBestSolution pb && minBound pb >= getBestCost pb
                    then
                        -- if the minimal bound is greater than the cost of the best solution -> cut,
                        --   we can't find a better solution descending from this one
                        return pb
                    else do
                        let path = findPath pb
                        if isJust path
                        then do
                            newProblem <- improveSolution (usePath pb $ fromJust path) endTime (i+1)
                            improveSolution (removePossiblility (copyBestSol pb newProblem) $ fromJust path) endTime (i+1)
                        else do
                            -- All path are avoided
                            return pb

{-
    Return Fasle if one or more node can't be filled with the actual configuration,
           True otherwise
    The problem could be unfeasible and still returns true, due to the ignored paths
    Does not assure that a path can be found to fill the edge
-}
isFeasible :: Problem -> Bool
isFeasible pb = foldl (\acc n -> acc && canFill n) True (Map.elems $ pb_nodes pb) where
    canFill :: Node -> Bool
    -- Check if the node n can be filled with the remaining capacity in the neighbour edges
    canFill n = (abs . n_b $ n) <= sum [e_r x| x <- Map.elems $ pb_edges pb, (e_start x == idx n || e_end x == idx n)]

{-
    Compute the minimal bound of a solution
-}
minBound :: Problem -> ValType
minBound pb = costSolution pb + (sum . map nodeEstimation . Map.elems $ pb_nodes pb) where
    nodeEstimation :: Node -> ValType
    nodeEstimation node = fillNode (abs $ n_b node) $ sortBy (\a b -> compare (cmup a) (cmup b)) edges where
        -- Compute the minimal cost to carry every ressource from a repository to a client
        cmup :: Edge -> ValType
        -- The cost if we carry the maximum number of ressources in an edge
        --        = (transhipment + unitary cost ) * number of product
        --           + fixed cost if the edge is not used
        cmup edge = (n_g $ getNode pb (e_start edge)) * fromIntegral (e_r edge)
            + e_h edge * fromIntegral (e_r edge)
            + if isNothing $ (idx edge) `Map.lookup` (pb_solution pb)
                then e_c edge   -- Not yet used -> Add the fixed cost
                else 0          -- Already used -> Already in the costSolution

        -- Return the edges starting / ending at node
        --   if ressources must by carried from / to node
        edges
            -- Repository -> Take the edge starting from the node
            | n_b node < 0 = [x | x <- Map.elems $ pb_edges pb, e_start x == idx node]
            -- Client -> Take the edge ending at the node
            | n_b node > 0 = [x | x <- Map.elems $ pb_edges pb, e_end x == idx node]
            -- finished / platform -> No need to carry anything
            | otherwise    = []

        -- Fill the node using the edges in the order they are given
        --   until there is no more ressources to carry
        fillNode :: Int -> [Edge] -> ValType
        fillNode nbr [] = 0
        fillNode nbr (x:xs) = if nbr <= 0
            then 0
            else
                -- Fixed cost
                (if e_a x==0 then e_c x else 0)

                 --  + Platforms
                + (n_g $ getNode pb (e_start x)) * fromIntegral nbr'

                 -- + Unitary cost
                + e_h x * fromIntegral nbr'

                 -- + Fill the rest
                + fillNode (nbr - nbr') xs where
                    nbr' = min nbr (e_r x)

{-
    Reduce the number of possiblility of a problem
      Remove a path from the possible path choice
-}
removePossiblility :: Problem -> Path -> Problem
removePossiblility pb path@(e1,e2, nbr) = pb {pb_remove = paths ++ (pb_remove pb)} where
    paths = [(e1, e2, x) | x <- [1..nbr]]

{-
    Return the best possible path to use
    Returned value : Just (id_edge 1, id_edge2, nbrProduct) | Nothing if there is no remaining path
-}
findPath :: Problem -> Maybe Path -- The first path, if exists, Nothing otherwise
findPath pb = if null res then Nothing else Just(minimumBy sortFunc res) where
    res = concat . map pathFrom $ Map.elems $ pb_nodes pb
    pathFrom :: Node -> [Path]
    pathFrom n = if n_b n >= 0 then [] --Get every possible transport from a repository node
        else concat . map pathWith $ Map.elems $ pb_edges pb where
            pathWith :: Edge -> [Path]
            pathWith e = if idx n == e_start e -- start from n
                            && e_a e <= e_u e -- can add product
                then catMaybes . map pathWith' $ Map.elems $ pb_edges pb
                else []
                where
                    pathWith' :: Edge -> Maybe Path
                    pathWith' e2 = if e_end e == e_start e2 -- Conected path

                                        && nbr > 0 -- Something to carry

                                        -- Time is correct
                                        && e_t e + e_t e2 + (n_s $ getNode pb (e_end e)) <= pb_maxTime pb

                                        -- not in the path to avoid
                                        && path `notElem` (pb_remove pb)
                        then Just path
                        else Nothing
                            where
                                path = (idx e, idx e2, nbr)
                                nbr = minimum [e_r e, -- Remaining capacity of the 1st edge
                                               e_r e2,-- Remaining capacity of the 2nd edge
                                               negate.n_b.getNode pb .e_start $ e, -- Number of product in the repository
                                               n_b.getNode pb.e_end $ e2] :: Int -- Number of product needed by the client

    -- Compare path according to the price it would cost to carry the products
    costOrder :: Path -> Path -> Ordering
    costOrder p1 p2 = compare (costPath p1) (costPath p2) where
        costPath :: Path -> ValType
        costPath (id_e1, id_e2, nbr) = (costEdge edge1 nbr) + (costEdge edge2 nbr) + n_g (getNode pb (e_end edge1)) where
            edge1 = getEdge pb id_e1
            edge2 = getEdge pb id_e2
            costEdge :: Edge -> Int -> ValType
            costEdge Edge{e_c=c, e_h=h, e_a=a} nbr = (if a == 0 then c else 0) + h*fromIntegral nbr

    -- Compare path according to the number of product ot could carry (the most is the best)
    quantityOrder :: Path -> Path -> Ordering
    -- Try to fill the demande of the clients as fast as possible
    quantityOrder (_, _, n1) (_, _, n2) = compare n2 n1

    -- If there is an intial solution
    -- then : Sort according to the costOrder to improve the solution
    -- else : Sort according to the quantityOrder to find a solution faster (and thus cut more solutions)
    sortFunc = costOrder --if hasBestSolution pb then costOrder else quantityOrder
