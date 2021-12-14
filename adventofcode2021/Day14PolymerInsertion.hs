import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative
import Data.Function ((&))

import qualified Data.Map.Strict as M

import Data.Char
import Data.List
import Safe

-- Boilerplate
dataPath = "data/Day14Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = do
	start <- many1 alphaNum
	newline
	newline
	rules <- endBy p_rule newline
	return (start, rules)
p_rule = do
	left <- count 2 anyChar
	string " -> "
	right <- anyChar
	return (left, right)

type Rule = (String, Char)

-- Part 1 (exponential)
p1 (s, rules) = score . (!!10) . iterate (step []) $ s where
	step ls     []     = reverse ls
	step []     (r:rs) = step [r] rs
	step (l:ls) (r:rs) | Just c <- lookup [l,r] rules = step (r:c:l:ls) rs
		
score s = last groups - head groups where
	groups = sort . map length . group . sort $ s
	
		
-- Part2
-- Each step, each pair creates to two new pairs. 
-- E.g. with a rule XZ -> Y then XZ -> XY + YZ (and the XZ disappears).
-- We can count the instances for each of the distinct pairs and not have to compute the string.
type PairCounts = M.Map String Integer
type Rule2 = (String, (String, String))

initCounts :: String -> PairCounts
initCounts s = M.fromList . map (\p -> (p, 1)) . pairs $ s where
	pairs (x:y:z) = [x,y]:(pairs (y:z)) 	
	pairs _ = []

p2 (seed, rules) = score2 . (!!40) . iterate step2 . initCounts $ seed where
	
	-- Transform each original rule of the form XZ -> Y 
	-- Into a more convenient pair XZ -> XY + YZ
	rules' :: [Rule2]
	rules' = map processRule rules
	processRule (s@[l,r], c) = (s, ([l,c], [c,r])) 
	
	-- Fold over all current pairs, generating a new map with the children pair counts
	-- For each existing pair with count v, add 2*v new pairs (v left, v right)
	step2 = M.foldrWithKey grow1 M.empty
	grow1 xz v pm = let (xy, yz) = lookupJust xz rules' in
		  M.insertWith (+) yz v
		. M.insertWith (+) xy v
		$ pm
		
	score2 pm = last groups - head groups where
		groups = pm  
			& M.toList
			& map (\([x,y],n) -> (x,n)) 
			& ((last seed, 1):) -- Add the last char, because only first chars are counted otherwise
			& sort 
			& H.groupOn fst 
			& map (sum . map snd)
			& sort
