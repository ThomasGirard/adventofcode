import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative

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

-- Part 1
p1 = score . (!!10) . grow

grow (s, rules) = iterate (step []) s where
	step ls [] = reverse ls
	step [] (r:rs) = step [r] rs
	step (l:ls) (r:rs) | Just c <- lookup [l,r] rules = step (r:c:l:ls) rs
		
score s = (last groups) - (head groups) where
	groups = sort . map length . group . sort $ s
		
-- This will grow like crazy...
-- There are only 10 unique characters and so 100 unique possible pairs to consider.
-- Each step, each pair gives birth to two other pairs. Say if we have a rule XZ -> Y
-- Then 1 XZ = 1 XY + 1 YZ (and the XZ disappears)
-- We can keep track of the 100 pairs as a count and not have to compute the exponential string.
type PairCounts = M.Map String Integer

p2 (s, rules) = score2 . (!!40) . iterate step2 . initCounts $ s where
	-- The last character must be added to the count, since we only count the first char of each pair
	score2 pm = last groups - head groups where
		groups = sort . map (sum . map snd) . H.groupOn fst . sort . ((last s,1):) . map (\([x,y],n) -> (x,n)) . M.toList $ pm
		
	rules2 = processRules rules
	
	-- Fold over all current pairs, generating a new map with the children pair counts
	step2 pm = M.foldrWithKey grow1 M.empty pm
	-- For each existing pair with count N, add 2*N new pairs (N left, N right)
	grow1 k v pm = let (a, b) = lookupJust k rules2 in
		M.alter (inc v) a . M.alter (inc v) b $ pm

inc v Nothing = Just v
inc v (Just x) = Just (v+x)

initCounts :: String -> PairCounts
initCounts s = M.fromList . map (\p -> (p, 1)) $ pairs where
	pairs = takeWhile ((==2) . length) . map (take 2) . tails $ s -- Not cute
		
-- Transform each original rule of the form XZ -> Y 
-- Into a more convenient pair XZ -> XY + YZ
processRules = map processRule
processRule (s@[l,r], c) = (s, ([l,c], [c,r])) 
type Rule2 = (String, (String, String))
	