import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.List
import Data.Char
import Data.Function(on)

-- Boilerplate
dataPath = "data/Day3Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all :: Parser [String]
p_all = endBy (many1 (oneOf "01")) newline


-- Part 1
p1 :: [String] -> Int
p1 bits = gamma * epsilon where
	half = length bits `div` 2
	gammaBits =	[H.boolToInt $ (sum . map digitToInt $ col) > half 
			| col <- transpose $ bits]
	epsilonBits = map (\x -> 1-x) gammaBits 
	gamma = bitsToInt gammaBits
	epsilon = bitsToInt epsilonBits

-- convert a binary number represented by a list of 0/1 digits to its decimal form
bitsToInt :: [Int] -> Int 
bitsToInt = foldl' (\n d -> n * 2 + d) 0


-- Part 2
p2 bits = oxy * co2 where
	bitsN = map (map digitToInt) bits
	oxyBits = partitionRecursive 0 Most bitsN
	co2Bits = partitionRecursive 0 Least bitsN
	oxy = bitsToInt oxyBits
	co2 = bitsToInt co2Bits

-- Recursively partition the set of bits, by looking at the Nth bit each round.
-- Pick the set to recurse on based on a Mode.
-- Stop when only one candidate remains
partitionRecursive depth mode bits = let
	(zeroes, ones) = partition (\num -> num!!depth == 0) bits
	chosen =  selectPartition mode zeroes ones
	in case chosen of
		[x] -> x
		xs -> partitionRecursive (depth+1) mode chosen

data Mode = Most | Least

-- Pick the right partition based on mode and partition lengths
-- In case of equal lengths, prefer ones for Most and vice versa.
selectPartition mode zeroes ones = case ((compare `on` length) zeroes ones, mode) of
	(LT, Most) -> ones
	(LT, Least) -> zeroes
	(GT, Most) -> zeroes
	(GT, Least) -> ones
	(EQ, Most) -> ones
	(EQ, Least) -> zeroes
	