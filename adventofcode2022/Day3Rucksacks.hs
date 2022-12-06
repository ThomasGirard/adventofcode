import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.Char(ord, isUpper, isLower)
import Data.List(intersect, nub)
import Data.List.Split(chunksOf)

-- Boilerplate
dataPath = "data/Day3Data.txt"
aoc_main = H.readAndParse dataPath H.p_lines
part1 = aoc_main p1
part2 = aoc_main p2

-- Solution
p1 = sum . map (prio . head . findCommon . splitHalves)
splitHalves xs = splitAt (length xs `div` 2) xs
findCommon (a, b) = a `intersect` b

prio x 
	| isLower x = ord x - (ord 'a') + 1
	| isUpper x = ord x - (ord 'A') + 27

p2 = sum . map (prio . findBadge) . chunksOf 3

findBadge [x,y,z] = case nub $ x `intersect` y `intersect` z of
	[b] -> b
	xs -> error $ "Non unique badge: " ++ (show xs)
