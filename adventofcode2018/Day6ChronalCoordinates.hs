import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.Char
import Data.List
import Control.Monad
import Data.Maybe

-- Boilerplate
dataPath = "data/6.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = endBy p_line newline
p_line = do
	x <- H.p_int
	string ", "
	y <- H.p_int
	return (x,y)

-- Part 1
type Coord = (Int, Int)

p1 :: [Coord] -> Int
p1 coords = largestSize mappedNoInf where
	-- Establish bounding box 
	minX = minimum . map fst $ coords
	maxX = maximum . map fst $ coords
	minY = minimum . map snd $ coords
	maxY = maximum . map snd $ coords
	mapped = [[closest coords (x,y) | x <- [minX..maxX]] | y <- [minY..maxY]]
	mappedNoInf = excludeInfinite mapped

closest :: [Coord] -> Coord -> Maybe Coord
closest coords c = let
	dists = sort . map (\t -> (manhattan c t, t)) $ coords
	minD = fst . head $ dists
	in case dists of 
		((_, cA):(dB, _):_) 
			| dB == minD -> Nothing -- Break ties with Nothing
			| otherwise -> Just cA

manhattan (x,y) (x',y') = (abs $ x - x') + (abs $ y - y')

-- Remove areas that are infinite (i.e. part of the border) and 
-- coordinates that are closest to more than one coordinate.
-- Infinite areas are those that extend to the border.
excludeInfinite :: [[Maybe Coord]] -> [Coord]
excludeInfinite mapped = filter (not . isBorder) . catMaybes . join $ mapped where
	borders = catMaybes . nub . join $ [head mapped, last mapped, head . transpose $ mapped, last . transpose $ mapped]
	isBorder = (`elem` borders)

largestSize = last . sort . map length . group . sort
	
p2 :: [Coord] -> Int
p2 coords = sum mapped where
	-- Establish bounding box
	minX = minimum . map fst $ coords
	maxX = maximum . map fst $ coords
	minY = minimum . map snd $ coords
	maxY = maximum . map snd $ coords
	mapped = [1 | x <- [minX..maxX], y <- [minY..maxY], isSafe coords (x,y)]
	
isSafe coords t = sum dists < 10000 where
	dists = [manhattan c t | c <- coords]
