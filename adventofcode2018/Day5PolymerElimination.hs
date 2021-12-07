import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.Char

-- Boilerplate
dataPath = "data/5.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = many1 alphaNum

-- Part 1
p1 = length . p1' []

p1' (l:ls) (r:rs) | isPair l r = p1' ls rs
p1' ls (r:rs) = p1' (r:ls) rs
p1' ls [] = ls

isPair a b = (isUpper a && toLower a == b) || (isUpper b && toLower b == a)

p2 s = minimum [length $ p2' x s | x <- ['a'..'z']]

-- Same as above but with one extra rule to consume the excluded letter pair
p2' z s = p2'' [] s where
	p2'' ls (r:rs) | r == z || toLower r == z = p2'' ls rs -- remove Z or z as it appears
	p2'' (l:ls) (r:rs) | isPair l r = p2'' ls rs
	p2'' ls (r:rs) = p2'' (r:ls) rs
	p2'' ls [] = ls

