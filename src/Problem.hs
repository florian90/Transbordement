module Problem (
    Problem(..),
    newProblem,
    evaluate,
    getNode,
    getEdge,
    isBestSolution,
    isSolutionOver,
    costSolution,
    hasBestSolution,
    newBestSolution,
    getBestCost,
    usePath,
    copyBestSol,
    getPb
)where

import System.IO

import Data.List
import Data.Maybe

import qualified Data.Map as Map

import Util
import Node
import Edge

data Problem = Problem {
    pb_name     :: String,

    pb_nbNode   :: Int,

    pb_nbEdge   :: Int,

    -- The maximum time to transport ressources
    pb_maxTime  :: ValType,

    pb_nodes    :: Map.Map ID Node,

    pb_edges    :: Map.Map ID Edge,

    -- Map with the edge ID and the number of product carried by the edge ID
    pb_solution :: Map.Map ID Int,

    -- The pb_solution of the best solution
    pb_bestSolution :: Map.Map ID Int,

    -- The cost of the pb_bestSolution
    pb_bestSolutionCost :: Maybe ValType,

    -- A list of path we can't use
    pb_remove :: [Path]
}

{-
    Return an empty problem
-}
newProblem = Problem "" 0 0 0 Map.empty Map.empty Map.empty Map.empty Nothing []

{-
instance Show Problem where
    show pb = pb_name pb ++ " : \n"
        ++ showMap (pb_nodes pb)
        ++ showMap (pb_edges pb)

{-
    Detailed version to see every detail of the problem
-}
showResult :: Problem -> String
showResult pb = pb_name pb ++ " : \n"
    ++ showMap (pb_nodes pb)
    ++ (foldl (\acc x -> acc ++ x) [] . map showEdgeUsage $ Map.elems $ pb_edges pb)
    ++ (if Map.null $ pb_bestSolution pb
            then ""
            else "Meilleure solution (cout=" ++ (show . fromJust . pb_bestSolutionCost) pb ++ ")= "
                ++ show (Map.assocs (pb_bestSolution pb)))
        where
            showEdgeUsage Edge{e_id=_id, e_start=start, e_end=end, e_u=u, e_c=c, e_h=h, e_t=t, e_a=a} =
                "Edge_" ++ show _id ++ " =\t["
                ++ "(" ++ show start ++ "->" ++ show end ++ ")"
                ++ ", u:" ++ show (if _id `Map.member` (pb_bestSolution pb) then pb_bestSolution pb Map.! _id else 0) ++ "/" ++ show u
                ++ ", c:" ++ show c
                ++ ", h:" ++ show h
                ++ ", t:" ++ show t ++ "]"
                ++ "\n"
-}

showCost :: Problem -> String
showCost pb = if hasBestSolution pb
    then "cost=" ++ (show . fromJust . pb_bestSolutionCost) pb
    else "No best solution founds"

{-
    Return an array of int, representing the number of element carried by the edges
    The n'th element represents the number of prodct carried by the edge of index n
    The first element is always 0 and represents nothing
-}
evaluate :: Problem -> [Int]
evaluate pb = map (\x -> fromMaybe 0 $ Map.lookup x (pb_bestSolution pb)) [0..pb_nbEdge pb]

{-
    Return the list of problem's edges
-}
getEdges :: Problem -> [Edge]
getEdges = Map.elems . pb_edges

{-
    Return the list of problem's nodes
-}
getNodes :: Problem -> [Node]
getNodes = Map.elems . pb_nodes

{-
    Return the edge of intex idx of the problem
-}
getEdge :: Problem -> ID -> Edge
getEdge Problem{pb_edges=edges} idx = edges Map.! idx

{-
    Return the node of intex idx of the problem
-}
getNode :: Problem -> ID -> Node
getNode Problem{pb_nodes=nodes} idx = nodes Map.! idx

{-
    Return the cost of the bes solution found, 0 if there is none
-}
getBestCost :: Problem -> ValType
getBestCost Problem{pb_bestSolutionCost=cost} = fromMaybe 0 cost

{-
    Check if the problem already have a solution
-}
hasBestSolution :: Problem -> Bool
hasBestSolution Problem{pb_bestSolutionCost=cost} = isJust cost

{-
    Check if the actual solution is better than the best solution
-}
isBestSolution :: Problem -> Bool
isBestSolution pb =  (not . hasBestSolution $ pb) || costSolution pb < getBestCost pb

{-
    Check if the actual solution transport every element from the repository to the clients
-}
isSolutionOver :: Problem -> Bool
isSolutionOver Problem {pb_maxTime=maxTime, pb_nodes=nodes, pb_edges=edges, pb_solution=sol} =
    foldl (\acc x -> if n_b x == 0 then acc else False) True $ Map.elems nodes
    -- If for every node the number of product is null : True, otherwise : False

{-
    Compute the cost of the current solution of a given problem
      Sum the cost of every edge used in the solution
-}
costSolution :: Problem -> ValType
costSolution Problem {pb_nodes=nodes, pb_edges=edges, pb_solution=sol} =
    foldl (\acc x -> acc + elemCost x ) 0 $ Map.assocs sol where
        elemCost :: (ID, Int) -> ValType
        elemCost (idEdge, nbrUtilisation)
            | nbrUtilisation == 0 = 0
            | otherwise = (e_c edge) + ((n_g startingNode) + (e_h edge)) * fromIntegral nbrUtilisation
                        --Fixed cost + (transshhipment cost + unitary cost) * nbr of unity carried
                where
                    edge = edges Map.! idEdge
                    startingNode = nodes Map.! (e_start edge)

{-
    Save the actual solution as the best solution, compute the cost of the best solution
-}
newBestSolution :: Problem -> IO Problem
newBestSolution pb = do
    let pb2 = pb {pb_bestSolution=pb_solution pb, pb_bestSolutionCost=Just(costSolution pb)}
    putStrLn $ "New solution : " ++ showCost pb2
    return pb2

{-
    Add a path to the problem, use the path to transfert ressources
-}
usePath :: Problem -> Path -> Problem
usePath pb (a, b, c) = (addTransport (addTransport pb b c) a c)


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
    Copy the best solution of the 2nd problem into the 1st one
-}
copyBestSol :: Problem -> Problem -> Problem
copyBestSol pb Problem {pb_bestSolution=best, pb_bestSolutionCost=cost} = pb{pb_bestSolution=best, pb_bestSolutionCost=cost}

{-
    Returns a IO Problem build according to a file
      The file must be in the directory data
    Param name : the name of the file
-}
getPb :: String -> IO Problem
getPb fileName = do
    putStrLn $ "Reading the file " ++ fileName ++ "..."
    file <- openFile ("../data/"++fileName) ReadMode
    lineReader file newProblem

{-
    Read a line from the file given in parameter
    Recursivly update the problem to build it
-}
lineReader :: Handle -> Problem -> IO Problem
lineReader file pb = do
    l <- hGetLine file
    if "EOF" `isPrefixOf` l
        then return pb
        else lineReader file (updateProblem l pb)

{-
    Update the problem with a string, which is a line from a data file
-}
updateProblem :: String -> Problem -> Problem
updateProblem str pb
    | "#" `isPrefixOf` str = pb
    | "NAME" `isPrefixOf` str       = setName pb (last $ words $ str)
    | "NBR_NODES" `isPrefixOf` str  = setNbNode pb $ str#(-1)
    | "NBR_EDGES" `isPrefixOf` str  = setNbEdge pb $ str#(-1)
    | "NODE" `isPrefixOf` str       = addNode pb (read str)
    | "EDGE" `isPrefixOf` str       = addEdge pb (read str)
    |  "T" `isPrefixOf` str         = setTime pb ( str#(-1))
    | otherwise                     = error "Unknow line during parsing the file"

-- ################ Setters to partialy build the problem #######################
{-
    Add an element to build the problem
-}

setName :: Problem -> String -> Problem
setName pb p_name = pb {pb_name=p_name}

setNbNode :: Problem -> Int -> Problem
setNbNode pb p_nbNode = pb {pb_nbNode=p_nbNode}

setNbEdge :: Problem -> Int -> Problem
setNbEdge pb p_nbEdge = pb {pb_nbEdge=p_nbEdge}

setTime :: Problem -> ValType -> Problem
setTime pb p_time = pb {pb_maxTime=p_time}

addNode :: Problem -> Node -> Problem
addNode pb node@Node{n_id=_id} = pb{pb_nodes = (Map.insert _id node (pb_nodes pb) ) }

addEdge :: Problem -> Edge -> Problem
addEdge pb edge@Edge{e_id=_id} = pb{pb_edges = Map.insert _id edge (pb_edges pb) }
