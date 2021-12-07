import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Control.Applicative
import Control.Monad(join)
import Data.List
import Data.List.Split(chunksOf)

-- Boilerplate
dataPath = "data/3.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = endBy p_line newline
p_line = do
	char '#'
	ruleNo <- H.p_int
	string " @ "
	x <- H.p_int
	char ','
	y <- H.p_int
	string ": "
	w <- H.p_int
	char 'x'
	h <- H.p_int
	return (ruleNo, (x,y),(w,h))
	
p1 patches = let touched = concatMap expand patches in
	H.countBy ((>1).length) . group . sort $ touched

expand (r,(sx,sy),(w,h)) = [(x,y) | x<-[sx..sx+w-1], y<-[sy..sy+h-1]]

p2 patches = let touchedOnce = map (\[x] -> x) . filter ((==1) . length) . group . sort . concatMap expand $ patches in
	head [r | patch@(r,_,_) <- patches, all (`elem` touchedOnce) (expand patch) ]
