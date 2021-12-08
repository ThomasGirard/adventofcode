import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

-- Boilerplate
dataPath = "data/Day7Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = sepBy H.p_int (char ',')

optimize costFun crabs = minimum [sum . map (costFun x) $ crabs | x <- [0..maximum crabs]]

p1 = optimize linear
linear x y = abs $ x - y

p2 = optimize quadratic
quadratic x y = let n = abs $ x - y in (n * (n+1)) `div` 2
