import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.Char
import Data.Maybe

import Control.Applicative
import Safe
import Data.List
import qualified Data.Set as S

-- Boilerplate
dataPath = "data/Day9Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all :: Parser [[Int]]
p_all = endBy p_line newline
p_line = many1 (digitToInt <$> digit)

-- Part 1
-- Iterate over the heightmap (zipping with row-column coordinates)
-- Compute the low points and sum them
p1 hm = sum . catMaybes . concat $ 
	[[lowPoint hm val r c | (val, c) <- zip row [0..]] | (row, r) <- zip hm [0..]]

lowPoint hm val r c = if all (>val) neighbors 
	then Just $ 1 + val
	else Nothing where
		neighbors = map readHm (neighborCoords r c)
		readHm (r, c) = atDef 10 (atDef [] hm r) c -- return 10 for out of bounds coordinates 

neighborCoords r c = [(r, c-1), (r, c+1), (r-1, c), (r+1, c)] 
		
-- Part 2
type Coord = (Int, Int)
type ToFill = S.Set Coord
	
p2 hm = product . take 3 . reverse . sort . map length $ basins where
	-- Create a set of all non-9 coordinates. These are the ones to fill.
	toFill = S.fromList $ [(r, c) | (row, r) <- zip hm [0..], (val, c) <- zip row [0..], val < 9]
	basins = fillBasins toFill
	
-- By picking any starting coordinate, and using a flood fill, we create a basin.
-- When the set is empty, all basins have been identified, and can be used to find the answer.
fillBasins :: ToFill -> [[Coord]]
fillBasins tf | S.null tf = []
fillBasins tf = newBasin : (fillBasins tf') where
	s = S.elemAt 0 tf
	(newBasin, tf') = fillBasin tf [] [s]
	
-- Flood fill a basin using coordinates available in ToFill only.
-- Do this recursively using a queue of coordinates to visit, 
-- Removing them from the set as they're visited, and queuing neighbors.
fillBasin :: ToFill -- Available coordinates
	-> [Coord] -- Identified as part of the basin 
	-> [Coord] -- Queued neighboring candidates 
	-> ([Coord], ToFill) -- Output the list of coordinates in the basin and the leftover available coordinates
fillBasin tf added [] = (added, tf)
fillBasin tf added ((r,c):cs) 
	| not $ S.member (r,c) tf = fillBasin tf added cs -- Ignore coordinates that are not available (either walls or out of bounds)
	| otherwise = fillBasin (S.delete (r,c) tf) ((r,c):added) (cs ++ neighborCoords r c)
