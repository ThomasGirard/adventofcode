import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.List(nub)

-- Boilerplate
dataPath = "data/Day6Data.txt"
aoc_main = H.readAndParse dataPath H.p_str
part1 = aoc_main p1
part2 = aoc_main p2

-- Solution
findMarker len k xs 
	| allUnique $ take len xs = k + len
	| otherwise = findMarker len (k+1) (tail xs)
	
allUnique x = nub x == x

p1 = findMarker 4 0
p2 = findMarker 14 0