import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

-- Boilerplate
dataPath = "data/Day6Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = sepBy H.p_int (char ',')

-- All fish can be represented as their day in the lifecycle between 0 and 8
-- Only the number of fish at each stage matters, not the individual fish.
-- Transform the initial data into a 9 element array where the n-th element
-- is the number of fish at stage n.
initDat dat = map fromIntegral [H.countBy (==x) dat | x <-[0..8]]

-- Step function, fish at stage 0 (a) 
-- * spawn new fish at stage 8 and
-- * go back to stage 6
-- All others shift down one stage.
step [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h+a,i,a]

-- Infinite list of generations
stepR x = x:(stepR (step x))

-- To solve either parts, take the K-th generation and sum
p1 :: [Int] -> Integer
p1 = sum . (!!80) . stepR . initDat

p2 :: [Int] -> Integer
p2 = sum . (!!256) . stepR . initDat