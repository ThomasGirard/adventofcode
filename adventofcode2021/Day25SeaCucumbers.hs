{-# LANGUAGE ViewPatterns #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Function ((&), on)

import qualified Data.Set as S

import Data.Char
import Data.List (sort, nub)
import Safe
import Debug.Trace
import Data.Maybe

-- Boilerplate
dataPath = "data/Day25Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1

-- Input parsing
p_all :: Parser [[Char]]
p_all = endBy (many1 (oneOf ".>v")) newline

type Coord = (Int, Int)

p1 :: [[Char]] -> Int
p1 input = 
	  (south, east)
	& steps
	& length
	where
	south = makeSet 'v' input
	east = makeSet '>' input

makeSet typ grid = S.fromList [ 
	(r,c) 
	| (row, r) <- zip grid [0..]
	, (x, c) <- zip row [0..]
	, x == typ
	]
		
steps herds = let herds' = step herds in
	if herds' == herds 
	then [herds]
	else herds : (steps herds')
	
step (south, east) = (south', east') where
	east'  = moveAll (0,1) (S.union south east ) east
	south' = moveAll (1,0) (S.union south east') south
	
moveAll dir blocked = S.map (moveTo blocked dir)
moveTo blocked (dr, dc) (r, c) = let (r',c') = ((r+dr) `mod` maxR, (c+dc) `mod` maxC) in
	if S.member (r',c') blocked
	then (r,c) 
	else (r', c')

-- Should be read from the input but christmas dinner won't wait
maxR = 137
maxC = 139

------------------
-- Debug / Display
------------------

display se = mapM_ putStrLn $ mapToGridSparse se

mapToGridSparse (south, east) = 
	[[readG (r,c) | c <- [0..maxC-1] ] | r <- [0..maxR-1]] where
	readG c = case (c `S.member` south, c `S.member` east) of
		(False, False) -> '.'
		(True, False) -> 'v'
		(False, True) -> '>'
		(True, True) -> '#' -- Life saver :)
