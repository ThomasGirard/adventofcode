import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.List(intersect, nub, transpose)
import Data.Maybe(catMaybes)
import Control.Applicative((<$))

import qualified Data.Vector as V

-- Boilerplate
dataPath = "data/Day5Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
-- TODO could be parsed from input
columns = 9
rows = 8

type Stack = [Char] -- head is the topmost crate
type Move = (Int, Int, Int) -- count, from, to

p_all :: Parser (V.Vector Stack, [Move])
p_all = do
	stacks <- p_stacks
	moves <- endBy p_move newline
	return (stacks, moves)
	
p_move = do
	string "move "
	count <- H.p_int
	string " from "
	from <- H.p_int
	string " to "
	to <- H.p_int
	return (count, from-1, to-1)

p_stacks = do
	stacks <- count rows p_stack_line
	string " 1   2   3   4   5   6   7   8   9 \n\n"
	return . mkStacks $ stacks

mkStacks = V.fromList . map catMaybes . transpose
	
p_stack_line = do
	crates <- count columns p_crate
	newline
	return crates
	
p_crate = do
	optional space
	crate <- choice [
		  p_crate'
		, Nothing <$ string "   "
		]
	return crate
	
p_crate' = do
	char '['
	crate <- letter
	char ']'
	return $ Just crate
	

-- Solution
p1 (stacks, moves) = readTop $ foldl doMove stacks moves 
doMove stacks (n, f, t) = foldl doMove1 stacks (take n $ repeat (f, t))
doMove1 stacks (f, t) = stacks V.// [(f, fs'), (t, ts')] where
	fs = stacks V.! f
	fs' = tail fs
	ts = stacks V.! t
	ts' = (head fs):ts

readTop = V.toList . V.map head
	
p2 (stacks, moves) = readTop $ foldl doMoveN stacks moves 
doMoveN stacks (n, f, t) = stacks V.// [(f, fs'), (t, ts')] where
	fs = stacks V.! f
	fs' = drop n fs
	ts = stacks V.! t
	ts' = (take n fs) ++ ts