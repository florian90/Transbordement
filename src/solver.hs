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

main :: IO ()
main = do
    args <- getArgs
    pb <- getPb $ args!!0
    putStrLn "Initial Problem : "
    --putStrLn $ show pb
    time <- Time.getCurrentTime >>= return . Time.utctDayTime
    sol <- improveSolution pb (time + 20*60) 0
    endTime <- Time.getCurrentTime >>= return . Time.utctDayTime
    --putStrLn "Final solution : "
    --putStrLn $ showResult sol
    putStrLn $ "Tab assignmenet : " ++ (show $ evaluate $ sol)
    putStrLn $ "Within " ++ show (endTime - time)
    return ()

improveSolution :: Problem -> Time.DiffTime -> Int -> IO Problem
improveSolution pb endTime i = do
    time <- Time.getCurrentTime >>= return . Time.utctDayTime

    --getChar
    --putStrLn "########## PROBLEM #########"
    --putStr $ show pb
    --putStrLn "############################"

    if endTime < time -- || hasBestSolution pb
        then do
            putStr "Temps écoulé "
            return pb
        else if not . isFeasible $ pb
            then do
                return pb
            else if isSolutionOver pb
                then if isBestSolution pb
                    then newBestSolution pb
                    else do
                        putStrLn $ (replicate i ' ') ++ "Return "
                        return pb
                else do
                    --putStrLn $ "minBound = " ++ (show . minBound $ pb)
                    if hasBestSolution pb && minBound pb >= getBestCost pb
                    then do
                        putStrLn $ (replicate i ' ') ++ "Cut " ++ show (getBestCost pb)
                        return pb
                    else do
                        let path = findPath pb
                        if isJust path
                        then do
                            --putStrLn $ (replicate i ' ') ++ findPath' pb
                            putStrLn $ (replicate i ' ') ++ "(" ++ (show i) ++ ") " ++ (show path) ++ " -> " ++ (show $ pb_remove pb)
                            newProblem <- improveSolution (usePath pb $ fromJust path) endTime (i+1)
                            putStrLn $ (replicate i ' ') ++ "(" ++ (show i) ++ ") "
                            improveSolution (removePossiblility (copyBestSol pb newProblem) $ fromJust path) endTime (i+1)
                        else do
                            -- All possible path are in pb_remove
                            putStrLn $ (replicate i ' ') ++ "No path"
                            return pb

{-
    Return Fasle if one or more node can't be filled with the actual configuration,
           True otherwise
    The problem could be unfeasible and still returns true, due to the path it can't use
-}
isFeasible :: Problem -> Bool
isFeasible pb = foldl (\acc n -> acc && canFill n) True (Map.elems $ pb_nodes pb) where
    canFill :: Node -> Bool
    canFill n = (abs . n_b $ n) <= sum [e_r x| x <- Map.elems $ pb_edges pb, (e_start x == idx n || e_end x == idx n)]

{-
    Compute the cost of the current solution of a given problem
-}
coutSolution :: Problem -> ValType
coutSolution Problem {pb_nodes=nodes, pb_edges=edges, pb_solution=sol} =
    foldl (\acc x -> acc + coutElem x ) 0 $ Map.assocs sol where
        coutElem :: (ID, Int) -> ValType
        coutElem (idEdge, nbrUtilisation)
            | nbrUtilisation == 0 = 0
            | otherwise = (e_c edge) + ((n_g startingNode) + (e_h edge)) * fromIntegral nbrUtilisation
                        --Fixed cost + (transshhipment cost + unitary cost) * nbr of unity carried
                where
                    edge = edges Map.! idEdge
                    startingNode = nodes Map.! (e_start edge)

{-
    add `nbr` elements on the edge of index `idx`
-}
addTransport :: Problem -> Int -> Int -> Problem
addTransport pb idx nbr = pb{pb_solution = Map.insert idx nbr' (pb_solution pb),
                             pb_edges = Map.insert idx (useEdge edge nbr') (pb_edges pb),
                             pb_nodes = move (pb_nodes pb) (e_start edge) (e_end edge) nbr}
        where
            edge = (pb_edges pb) Map.! idx
            nbr' = if Map.member idx $ pb_solution pb then nbr + pb_solution pb Map.! idx else nbr
            useEdge :: Edge -> Int -> Edge
            useEdge edge nbr = edge{e_a = nbr}
            move :: Map.Map Int Node -> Int -> Int -> Int -> Map.Map Int Node
            move map_edges from to nbr = Map.insert from (changeCapacityNode from nbr) (Map.insert to (changeCapacityNode to (-nbr)) map_edges) where
                changeCapacityNode :: Int -> Int -> Node
                changeCapacityNode n_idx nbr = node{n_b=(n_b node) + nbr} where
                    node = getNode pb n_idx

{-
    Check if the actual solution transport every element from the repository to the clients
-}
isSolutionOver :: Problem -> Bool
isSolutionOver Problem {pb_maxTime=maxTime, pb_nodes=nodes, pb_edges=edges, pb_solution=sol} =
    foldl (\acc x -> if n_b x == 0 then acc else False) True $ Map.elems nodes
    -- If for every node the number of product is null : True, otherwise : False

{-
    Save the actual solution as the best solution, compute the cost of the best solution
-}
newBestSolution :: Problem -> IO Problem
newBestSolution pb = do
    let pb2 = pb {pb_bestSolution=pb_solution pb, pb_bestSolutionCost=Just(coutSolution pb)}
    putStrLn $ "New solution : " ++ showSolution pb2
    return pb2

{-
    Check if the actual solution is better than the actual best solution
-}
isBestSolution :: Problem -> Bool
isBestSolution pb =  (not . hasBestSolution $ pb) || coutSolution pb < getBestCost pb

{-
    Check if the problem already have a solution
-}
hasBestSolution :: Problem -> Bool
hasBestSolution Problem{pb_bestSolutionCost=cost} = isJust cost

{-
    Return the cost of the bes solution found, 0 if there is none
-}
getBestCost :: Problem -> ValType
getBestCost Problem{pb_bestSolutionCost=cost} = fromMaybe 0 cost

{-
    Compute the minimal bound of a solution
-}
minBound :: Problem -> ValType
minBound pb = coutSolution pb + (sum . map nodeEstimation . Map.elems $ pb_nodes pb) where
    nodeEstimation :: Node -> ValType
    nodeEstimation node = fillNode (abs $ n_b node) $ sortBy (\a b -> compare (cmup a) (cmup b)) edges where
        cmup :: Edge -> ValType
        cmup edge = (n_g $ getNode pb (e_start edge)) * fromIntegral (e_r edge)
            + e_h edge * fromIntegral (e_r edge)
            + if isNothing $ (idx edge) `Map.lookup` (pb_solution pb)
                then e_c edge   -- Si pas encore utilisé
                else 0          -- si déjà utilisé
        edges = if n_b node < 0 --Repository
            then [x | x <- Map.elems $ pb_edges pb, e_start x == idx node]
            else if n_b node > 0 -- Client
                then [x | x <- Map.elems $ pb_edges pb, e_end x == idx node]
                else [] -- finished / platform
        fillNode :: Int -> [Edge] -> ValType
        fillNode nbr [] = 0
        fillNode nbr (x:xs) = if nbr <= 0
            then 0
            else (if e_a x==0 then e_c x else 0) --fixed cost
                + (n_g $ getNode pb (e_start x)) * fromIntegral nbr' --transportation platforms
                + e_h x * fromIntegral nbr' --fixed cost
                + fillNode (nbr - nbr') xs where -- Fill the rest of the quantity in de repository
                    nbr' = min nbr (e_r x)

{-
    Reduce the number of possiblility of a problem
-}
removePossiblility :: Problem -> Path -> Problem
removePossiblility pb path = pb {pb_remove = path:(pb_remove pb)}

{-
    Return the best possible path to use
    Returned value : Just (id_edge 1, id_edge2, nbrProduct) | Nothing if there is no remaining path
-}
findPath :: Problem -> Maybe Path
findPath pb = if null res then Nothing else Just(minimumBy sortFunc res) where
    res = concat . map pathFrom $ Map.elems $ pb_nodes pb
    pathFrom :: Node -> [Path]
    pathFrom n = if n_b n >= 0 then [] --Get every possible transport from a repository node
        else concat . map pathWith $ Map.elems $ pb_edges pb where
            pathWith :: Edge -> [Path]
            pathWith e = if idx n == e_start e -- start from n
                            && e_a e <= e_u e -- can add product
                then filter (`notElem` (pb_remove pb)) . catMaybes . map pathWith' $ Map.elems $ pb_edges pb
                else []
                where
                    pathWith' :: Edge -> Maybe Path
                    pathWith' e2 = if e_end e == e_start e2      --conected path
                                        && nbr > 0               --something to carry
                                        && time <= pb_maxTime pb --time is correct
                        then Just(idx e, idx e2, nbr) else Nothing
                        where
                            nbr = minimum [e_r e, e_r e2, negate.n_b.getNode pb .e_start $ e, n_b.getNode pb.e_end $ e2] :: Int
                            time = e_t e + e_t e2 + (n_s $ getNode pb (e_end e))
    costOrder :: Path -> Path -> Ordering
    costOrder p1 p2 = compare (costPath p1) (costPath p2) where
        costPath :: Path -> ValType
        costPath (id_e1, id_e2, nbr) = (costEdge edge1 nbr) + (costEdge edge2 nbr) + n_g (getNode pb (e_end edge1)) where
            edge1 = getEdge pb id_e1
            edge2 = getEdge pb id_e2
            costEdge :: Edge -> Int -> ValType
            costEdge Edge{e_c=c, e_h=h, e_a=a} nbr = (if a == 0 then c else 0) + h*fromIntegral nbr
    quantityOrder :: Path -> Path -> Ordering
    quantityOrder (_, _, n1) (_, _, n2) = compare n2 n1
    sortFunc = if hasBestSolution pb then costOrder else quantityOrder

findPath' :: Problem -> String
findPath' pb = show res where
    res = concat . map pathFrom $ Map.elems $ pb_nodes pb
    pathFrom :: Node -> [Path]
    pathFrom n = if n_b n >= 0 then [] --Get every possible transport from a repository node
        else concat . map pathWith $ Map.elems $ pb_edges pb where
            pathWith :: Edge -> [Path]
            pathWith e = if idx n == e_start e -- start from n
                            && e_a e <= e_u e -- can add product
                then filter (`notElem` (pb_remove pb)) . catMaybes . map pathWith' $ Map.elems $ pb_edges pb
                else []
                where
                    pathWith' :: Edge -> Maybe Path
                    pathWith' e2 = if e_end e == e_start e2      --conected path
                                        && nbr > 0               --something to carry
                                        && time <= pb_maxTime pb --time is correct
                        then Just(idx e, idx e2, nbr) else Nothing
                        where
                            nbr = minimum [e_r e, e_r e2, negate.n_b.getNode pb .e_start $ e, n_b.getNode pb.e_end $ e2] :: Int
                            time = e_t e + e_t e2 + (n_s $ getNode pb (e_end e))
    costOrder :: Path -> Path -> Ordering
    costOrder p1 p2 = compare (costPath p1) (costPath p2) where
        costPath :: Path -> ValType
        costPath (id_e1, id_e2, nbr) = (costEdge edge1 nbr) + (costEdge edge2 nbr) + n_g (getNode pb (e_end edge1)) where
            edge1 = getEdge pb id_e1
            edge2 = getEdge pb id_e2
            costEdge :: Edge -> Int -> ValType
            costEdge Edge{e_c=c, e_h=h, e_a=a} nbr = (if a == 0 then c else 0) + h*fromIntegral nbr
    quantityOrder :: Path -> Path -> Ordering
    quantityOrder (_, _, n1) (_, _, n2) = compare n2 n1
    sortFunc = if hasBestSolution pb then costOrder else quantityOrder

{-
    Return the remaining capacity of the edge of index `edgeIdx` in the Problem pb
-}
remainingCapacity :: Problem -> Int -> Int
remainingCapacity pb edgeIdx = if isNothing mb_usage
    then capacity
    else capacity - fromJust mb_usage
    where
        capacity = e_u $ (pb_edges pb) Map.! edgeIdx :: Int
        mb_usage = edgeIdx `Map.lookup` (pb_solution pb) :: Maybe Int

{-
    add a path to solutions
-}
usePath :: Problem -> Path -> Problem
usePath pb (a, b, c) = (addTransport (addTransport pb b c) a c)

{-
    Copy the best solution of the 2nd problem into the 1st one
-}
copyBestSol :: Problem -> Problem -> Problem
copyBestSol pb Problem {pb_bestSolution=best, pb_bestSolutionCost=cost} = pb{pb_bestSolution=best, pb_bestSolutionCost=cost}
