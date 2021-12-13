import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.List

-- Boilerplate
dataPath = "data/2.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = endBy (many1 alphaNum) newline

-- Part 1
p1 dat = (g 2) * (g 3) where 
	grouped = map (map (\g -> (head g, length g)) . group . sort) $ dat
	g n = length . filter (any ((== n) . snd)) $ grouped
	
p2 dat = head [diff x y | x<-dat, y<-dat, x<y, dist x y == 1]
	
dist a b = H.countBy (uncurry (/=)) $ zip a b
diff a b = map fst . filter (uncurry (==)) $ zip a b
