import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.List(sort)

-- Boilerplate
dataPath = "data/Day2Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all :: Parser [(Char, Char)]
p_all = endBy p_row newline

p_row = do
	l <- oneOf "ABC" -- RPS
	space
	r <- oneOf "XYZ" -- RPS
	return (l, r)
	
-- Solution
p1 = sum . map score

score (l, r) = scorePair l r + scoreYour r

scorePair 'A' 'X' = 3
scorePair 'A' 'Y' = 6
scorePair 'A' 'Z' = 0
scorePair 'B' 'X' = 0
scorePair 'B' 'Y' = 3
scorePair 'B' 'Z' = 6
scorePair 'C' 'X' = 6
scorePair 'C' 'Y' = 0
scorePair 'C' 'Z' = 3

scoreYour 'X' = 1
scoreYour 'Y' = 2
scoreYour 'Z' = 3

p2 = sum . map score . map changePlay

changePlay (l, r) = let r' = findPlay l r in (l, r')

findPlay 'A' 'X' = 'Z'
findPlay 'A' 'Y' = 'X'
findPlay 'A' 'Z' = 'Y'
findPlay 'B' 'X' = 'X'
findPlay 'B' 'Y' = 'Y'
findPlay 'B' 'Z' = 'Z'
findPlay 'C' 'X' = 'Y'
findPlay 'C' 'Y' = 'Z'
findPlay 'C' 'Z' = 'X'
