import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.List(sort)

-- Boilerplate
dataPath = "data/Day1Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all :: Parser [[Int]]
p_all = do many1 p_elf

p_elf = do
	calories <- endBy1 H.p_int newline
	optional newline
	return calories
		
-- Solution
p1 :: [[Int]] -> Int
p1 = last . sort . map sum

p2 :: [[Int]] -> Int
p2 = sum . take 3 . reverse . sort . map sum

-- Rewritten using a common helper function
p1' = topN 1
p2' = topN 3

topN :: Int -> [[Int]] -> Int
topN n = sum . take n . reverse . sort . map sum
