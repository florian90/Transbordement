module Solver where

import Prelude hiding (lookup, minBound)

import Data.List (sortBy)

import System.Environment

import Data.Bool
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Time as Time

import Util
import Node
import Edge
import Problem
import Solution

main :: IO ()
main = do
    args <- getArgs
    pb <- getPb $ args!!0
    putStrLn "Initial Problem : "
    putStrLn $ show pb
    time <- Time.getCurrentTime >>= return . Time.utctDayTime
    sol <- improveSolution pb (emptySolution pb) (time + 20*60) 0
    endTime <- Time.getCurrentTime >>= return . Time.utctDayTime
    --putStrLn "Final solution : "
    --putStrLn $ showResult sol
    putStrLn $ "Tab assignmenet : " ++ (show $ sol)
    putStrLn $ "Within " ++ show (endTime - time)
    return ()

improveSolution :: Problem -> Solution -> Time.DiffTime -> Int -> IO Solution
improveSolution pb sol endTime i = do
    time <- Time.getCurrentTime >>= return . Time.utctDayTime

    --getChar
    --putStrLn "########## PROBLEM #########"
    --putStr $ show pb
    putStrLn "############################"

    if endTime < time -- || hasBestSolution pb
        then do
            --putStr "Temps écoulé "
            return sol
        else if not $ isFeasible pb sol
            then do
                putStrLn "Unfeasible solution"
                return sol
            else if isSolutionOver sol
                then do
                    putStrLn "Solution over !"
                    newSolution pb sol
                else do
                    putStrLn $ "minBound = " ++ (show $ minBound pb sol)
                    if hasBestSolution sol && minBound pb sol >= getBestCost sol
                    then do
                        putStrLn $ "Cut " ++ show (getBestCost sol)
                        return sol
                    else do
                        let path = findPath pb sol
                        putStrLn $ "Path : " ++ show path
                        --putStrLn $ "(" ++ (show i) ++ ") " ++ (show path) ++ " -> " ++ (show $ sol_remove sol)
                        if isJust path
                        then do
                            let sol' = (usePath pb sol $ fromJust path)
                            putStrLn $ showResult pb sol'
                            newSol <- improveSolution pb sol' endTime (i+1)
                            improveSolution pb (removePossiblility (copyBestSol sol newSol) $ fromJust path) endTime (i+1)
                        else do
                            putStrLn "Other"
                            return sol;

{-
    Return Fasle if one or more node can't be filled with the actual configuration,
           True otherwise
    /!\ should take care if path can still be taken
-}
isFeasible :: Problem -> Solution -> Bool
isFeasible pb sol = foldl (\acc n -> acc && canFill n) True (Map.assocs . sol_nodes $ sol) where
                    -- For each node, check if we can empty / fill the node
    canFill :: (ID, Int) -> Bool
    canFill (index, b_node) = (b_node == 0) || (abs b_node) <=
        sum [remainingCapacity pb sol (idx e) | e <- pb_edges pb, (e_start e == index || e_end e == index)];
            --if PF -> OK? otherwise, find if there is still enough capacity in the edges to bring / evacuate ressources

{-
    Compute the cost of the sol_edges of a given problem
-}
cout :: Problem -> Solution -> ValType
cout Problem {pb_nodes=nodes, pb_edges=edges} Solution{sol_edges=edgeSol} =
    foldl (\acc x -> acc + coutElem x) 0 $ Map.assocs edgeSol where
        coutElem :: (ID, Int) -> ValType
        coutElem (idEdge, nbrUtilisation)
            | nbrUtilisation == 0 = 0
            | otherwise = (e_c edge) + ((n_g node) + (e_h edge)) * fromIntegral nbrUtilisation
                where
                    edge = edges!!idEdge
                    node = nodes!!(e_start edge)

{-
    add `nbr` elements on the edge of index `idx`
-}
addTransport :: Problem -> Solution -> Int -> Int -> Solution
-- addTransport pb sol index nbr = sol{sol_edges = Map.insert index (nbr + (sol_edges sol) Map.! index) modified_sol_edges,
--                                     sol_nodes = Map.insert from (nbr + (sol_nodes sol) Map.! from) (sol_nodes sol)} where
--                                         edgePb = (pb_edges pb)!!index   :: Edge
--                                         from = e_start edgePb           :: ID
--                                         to = e_end edgePb               :: ID
--                                         modified_sol_edges = Map.insert to ((sol_nodes sol) Map.! to-nbr) (sol_edges sol)
{-
    Ajouter nbr à l'edge idx
    Ajouter nbr au node e_start edge
    enlever nb au node e_end edge
-}
addTransport pb sol index nbr = sol{sol_edges = Map.insertWith (+) index nbr (sol_edges sol),
                                    sol_nodes = Map.insertWith (+) from nbr (Map.insertWith (-) to nbr (sol_nodes sol))
                                    } where
                                        edgePb = (pb_edges pb)!!index   :: Edge
                                        from = e_start edgePb           :: ID
                                        to = e_end edgePb               :: ID

{-
    Check if the actual solution transport every element from the repository to the clients
-}
isSolutionOver :: Solution -> Bool
isSolutionOver Solution{sol_nodes=nodes} =
    foldl (\acc x -> if x == 0 then acc else False) True nodes
    -- If for every node the number of product is null : True,
    --    otherwise : False

{-
    Save the actual solution as the best solution, compute the cost of the best solution
-}
newSolution :: Problem -> Solution -> IO Solution
newSolution pb sol = if isBestSolution pb sol
    then do
        let sol2 = sol {sol_best=sol_edges sol, sol_bestCost=Just(cout pb sol)}
        putStrLn $ "New solution : " ++ showSolution sol2
        return sol2
    else return sol

{-
    Check if the actual solution is betten than the actual best solution
-}
isBestSolution :: Problem -> Solution -> Bool
isBestSolution pb sol =  (not . hasBestSolution $ sol) || cout pb sol < getBestCost sol

hasBestSolution :: Solution -> Bool
hasBestSolution sol = isJust . sol_bestCost $ sol

{- -- #############################################################################
    Compute the minimal bound of a solution
-}
minBound :: Problem -> Solution -> ValType
minBound pb sol = cout pb sol + (sum . map nodeEstimation $ pb_nodes pb) where
        nodeEstimation :: Node -> ValType
        nodeEstimation node = fillNode (abs $ (getNode' sol (idx node))) $ sortBy (\a b -> compare (cmup a) (cmup b)) edges where
            cmup :: Edge -> ValType
            cmup edge = (n_g $ getNode pb (e_start edge)) * fromIntegral (remainingCapacity pb sol (idx edge))
                + e_h edge * fromIntegral (remainingCapacity pb sol (idx edge))
                + if isNothing $ (idx edge) `Map.lookup` (sol_edges sol)
                    then e_c edge   -- Si pas encore utilisé
                    else 0          -- si déjà utilisé
            edges = if (getNode' sol (idx node)) < 0 --Repository
                then [x | x <- pb_edges pb, e_start x == idx node]
                else if (getNode' sol (idx node)) > 0 -- Client
                    then [x | x <- pb_edges pb, e_end x == idx node]
                    else [] -- finished / platform
            fillNode :: Int -> [Edge] -> ValType
            fillNode nbr [] = 0
            fillNode nbr (x:xs) = if nbr <= 0
                then 0
                else (if (getEdge' sol (idx x))==0 then e_c x else 0) --fixed cost
                    + (n_g $ getNode pb (e_start x)) * fromIntegral nbr' --transportation platforms
                    + e_h x * fromIntegral nbr' --fixed cost
                    + fillNode (nbr - nbr') xs where -- Fill the rest of the quantity in de repository
                        nbr' = min nbr (remainingCapacity pb sol (idx x));

{-
    Reduce the number of possiblility of a problem
-}
removePossiblility :: Solution -> Path -> Solution
removePossiblility sol path@(a, b, nbr) = sol {sol_remove = path:(sol_remove sol)}{-,pb_edges= replace (idx lower) (decrement lower) (pb_edges pb)} where
    ea = (pb_edges pb)!!a :: Edge
    eb = (pb_edges pb)!!b :: Edge
    lower = if pb `remainingCapacity` a <= pb `remainingCapacity` b then ea else eb :: Edge
    decrement :: Edge -> Edge
    decrement e = e{e_u=0}-}

{-
    Return the best possible path to use
    Returned value : Just (id_edge 1, id_edge2, nbrProduct) | Nothing if there is no remaining path
-}
findPath :: Problem -> Solution -> Maybe Path
findPath pb sol = if null res then Nothing else Just(res!!0) where
    res = sortBy costOrder . concat . map pathFrom $ pb_nodes pb
    pathFrom :: Node -> [Path]
    pathFrom n = if (getNode' sol (idx n)) >= 0 then [] --Get every possible path from the repositories
        else concat . map pathWith $ pb_edges pb where
            pathWith :: Edge -> [Path]
            pathWith e = if idx n == e_start e -- start from n
                        && (getEdge' sol (idx e)) <= e_u e -- can add product
                then filter (`notElem` (sol_remove sol)) . catMaybes . map pathWith' $ pb_edges pb
                else []
                where
                    pathWith' :: Edge -> Maybe Path
                    pathWith' e2 = if e_end e == e_start e2      --conected path
                                        && nbr > 0               --something to carry
                                        && time <= pb_maxTime pb --time is correct
                        then Just(idx e, idx e2, nbr) else Nothing
                        where
                            nbr = minimum  [remainingCapacity pb sol (idx e),
                                            remainingCapacity pb sol (idx e2),
                                            negate.getNode' sol . e_start $ e,
                                            getNode' sol . e_end $ e2] :: Int
                            time = e_t e + e_t e2 + (n_s $ getNode pb (e_end e))
    costOrder :: Path -> Path -> Ordering
    costOrder p1 p2 = compare (costPath p1) (costPath p2) where
        costPath :: Path -> ValType
        costPath (id_e1, id_e2, nbr) = (costEdge id_e1 nbr) + (costEdge id_e2 nbr) + n_g (getNode pb (e_end $ getEdge pb id_e1)) where
                        -- Cost from repository to p.f. + cost from p.f. to client + transshipment cost
            costEdge :: Int -> Int -> ValType
            costEdge id_edge nbr = (if a == 0 then e_c edge else 0) + (e_h edge)*fromIntegral nbr where
                a = getEdge' sol id_edge
                edge = getEdge pb id_edge
    costOrder' :: Path -> Path -> Ordering
    costOrder' (i11, i21, n1) (i12, i22, n2) = compare n1 n2;

{-
    Return the remaining capacity of the edge of index `edgeIdx` in the Problem pb
-}
remainingCapacity :: Problem -> Solution -> ID -> Int
remainingCapacity Problem{pb_edges=pbEdges} Solution{sol_edges=solEdges} edgeIdx = capacity - usage where
    capacity = e_u $ pbEdges!!edgeIdx
    usage = fromMaybe 0 $ edgeIdx `Map.lookup` solEdges; -- 0 if not used, it's value otherwise

{-
    add a path to solutions
-}
usePath :: Problem -> Solution -> Path -> Solution
usePath pb sol (a, b, c) = (addTransport pb (addTransport pb sol b c) a c)

copyBestSol :: Solution -> Solution -> Solution
copyBestSol sol Solution {sol_best=best, sol_bestCost=cost} = sol{sol_best=best, sol_bestCost=cost}

showResult :: Problem -> Solution -> String
showResult pb sol = show (sol_edges sol)
