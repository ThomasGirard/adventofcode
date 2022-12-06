import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.Char(ord, isUpper, isLower)
import Data.List(intersect, nub)
import Data.List.Split(chunksOf)

-- Boilerplate
dataPath = "data/Day4Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
type Coord = (Int, Int)
p_all :: Parser [(Coord, Coord)]
p_all = endBy p_line newline

p_line = do
	a <- p_coord
	char ','
	b <- p_coord
	return (a, b)
	
p_coord = do
	l <- H.p_int
	char '-'
	r <- H.p_int
	return (l, r)
	
-- Solution
p1 = length . filter isContained
p2 = length . filter isOverlap

isContained (l, r) = (go l r) || (go r l) where
	go (x, y) (a, b) = x <= a && y >= b
isDisjoint (l, r) = (go l r) || (go r l) where
	go (x, y) (a, b) = a > y || x > b
isOverlap = not . isDisjoint
