import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative

-- Boilerplate
dataPath = "data/1.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = endBy p_line newline
p_line = do
	sign <- choice [
		id <$ char '+',
		negate <$ char '-' 
		]
	val <- H.p_int
	return $ sign val

-- Part 1
p1 = sum

p2 = firstDup [] . scanl (+) 0 . cycle
firstDup seen (x:xs) 
	| x `elem` seen = x
	| otherwise = firstDup (x:seen) xs
