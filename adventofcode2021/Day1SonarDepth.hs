import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

-- Boilerplate
dataPath = "data/Day1Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all :: Parser [Int]
p_all = endBy H.p_int newline
		
-- Solution
p1 :: [Int] -> Int
p1 ds = H.countBy (uncurry (<)) . zip ds $ (drop 1 ds)

p2 :: [Int] -> Int
p2 ds = H.countBy (uncurry (<)) . zip ds $ (drop 3 ds)