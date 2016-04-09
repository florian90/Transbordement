import System.Random
import Data.Char
import Data.List
import Control.Monad
import System.IO

--import qualified Data.Map as Map

{-
	putStrLn "Hello, World\n"
	putStrLn "Name : "
	getLine
-}

test1 = do
		putStrLn "Hello, World"
		putStr "Quel est votre nom ? : "
		x <- getLine
		putStrLn $ "Bonjour " ++ x
		return x

plusMoins x min max coupRestant = do
	if coupRestant <= 0
		then putStrLn $ "Nombre de coups max écoulés... le nombre était : " ++ show x
		else do
			putStrLn $ "Entrez un nombre entre " ++ show min ++ " et " ++ show max
			putStr ">"
			y <- readLn
			case compare x y of
				LT -> do
					putStrLn $ "Le nombre mysère est plus petit que " ++ show y
					plusMoins x min (y-1) (coupRestant-1)
				GT -> do
					putStrLn $ "Le nombre mysère est plus grand que " ++ show y
					plusMoins x (y+1) max (coupRestant-1)
				EQ -> do
					putStrLn $ "Bravo, c'était " ++ show x

jeu min max nbCoups = do
	x <- randomRIO (min,max)
	plusMoins x min max nbCoups

infToUpper = do
  l <- getLine
  putStrLn $ map toUpper l
  infToUpper

main = do
	file <- openFile "../data/transshipment1.txt" ReadMode
	rdAllFile file

rdAllFile :: Handle => IO()
rdAllFile file = do
	l <- hGetLine file
	putStrLn l
	unless ("EOF" `isPrefixOf` l) $ do rdAllFile file
