import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Control.Monad
import Control.Applicative
import Safe
import Data.List

-- Boilerplate
dataPath = "data/Day8Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = endBy p_line newline
p_line = do
	zeroNine <- count 10 (p_word <* space)
	char '|'
	display <- count 4 (space >> p_word)
	return (zeroNine, display)
p_word = many1 (oneOf "abcdefg")	

-- Part 1
p1 = H.countBy ((`elem` [2,3,4,7]) . length). join . map snd

-- Part 2
p2 = sum . map p2_line

p2_line (zeroNine, display) =  digitsToInt . map convert $ display where
	table = makeMapping zeroNine 
	convert = realToInt . sort . map toReal
	toReal c = lookupJust c table

digitsToInt = foldl' (\n d -> n * 10 + d) 0

-- Based on the zero to nine scrambled digits, 
-- create a mapping from the input symbols (a..g) to "real" symbols
--   T
--  L R
--   M
--  A Z 
--   B
-- The deduction works by looking at a combination of 
-- Frequency of occurences of scrambled symbols (byFrequency), some symbols appear a unique number of times and can be deduced straight away (A,L,Z).
-- Length of specific digits (e.g. one having only 2 signals). When the first 3 symbols have been
-- deduced by frequency, the others can be deduced by successive exclusion in specific digits.
makeMapping :: [String] -> [(Char, Char)]
makeMapping xs = [(t,'T'),(l,'L'),(r,'R'),(m,'M'),(a,'A'),(z,'Z'),(b,'B')] where
	byFrequency = map head . sortOn length . group . sort . join $ xs 
	byLength = sortOn length xs
	a = byFrequency !! 0
	l = byFrequency !! 1
	z = last byFrequency
	[r] = byLength!!0 \\ [z] -- In one
	[t] = byLength!!1 \\ [z,r] -- In seven
	[m] = byLength!!2 \\ [l,r,z] -- In four
	[b] = byLength!!7 \\ [t,l,r,m,a,z] -- In eight

-- Convert a string of real symbols, to the matching digit.
-- The string MUST be sorted.
realToInt :: String -> Int
realToInt "ABLRTZ" = 0
realToInt "RZ" = 1
realToInt "ABMRT" = 2
realToInt "BMRTZ" = 3
realToInt "LMRZ" = 4
realToInt "BLMTZ" = 5
realToInt "ABLMTZ" = 6
realToInt "RTZ" = 7
realToInt "ABLMRTZ" = 8
realToInt "BLMRTZ" = 9
	