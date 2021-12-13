import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative

import qualified Data.Set as S

import Data.Char
import Data.List ((\\), elemIndices, intersperse, group, sort, transpose)

-- Boilerplate
dataPath = "data/Day13Data.txt"
aoc_main = H.readAndParse dataPath p_all

-- Input parsing
p_all = do
	coords <- endBy p_coord newline
	newline
	folds <- endBy p_fold newline
	return (S.fromList coords, folds)
p_coord = do
	x <- H.p_int
	char ','
	y <- H.p_int
	return (x,y)
p_fold = do
	string "fold along "
	fold <- choice [
		XFold <$ char 'x', 
		YFold <$ char 'y']
	char '='
	val <- H.p_int
	return $ fold val
	
data Fold = XFold Int | YFold Int deriving (Ord, Eq, Show)
type Coord = (Int, Int)
type PointSet = S.Set Coord 

-- Part 1
part1 = aoc_main p1
p1 (points, f:folds) = S.size $ ffold f points

ffold :: Fold -> PointSet -> PointSet
ffold (XFold n) = S.map (\(x,y) -> (foldcoord n x, y))  
ffold (YFold n) = S.map (\(x,y) -> (x, foldcoord n y)) 


foldcoord fold n = case compare n fold of 
	LT -> n
	EQ -> error "Should not happen"
	GT -> 2*fold - n

-- Part 2
part2 = aoc_main p2 >>= mapM_ putStrLn
p2 (points, folds) = toString $ foldl (flip ffold) points folds

toString :: PointSet -> [String]
toString points = 
	[[if (x,y) `S.member` points then '#' else ' ' | x <- [0..maxX] ] | y <- [0..maxY]] where
		maxX = maximum $ S.map fst points 
		maxY = maximum $ S.map snd points 
