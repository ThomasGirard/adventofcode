{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered
import qualified Data.Set as S
import Data.Bits

import Control.Applicative ((*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char

import Debug.Trace (trace)

p_all :: Parser [[Int]]
p_all = endBy p_line newline
p_line = many1 p_cube
p_cube = choice [
		0 <$ (char '.'),
		1 <$ (char '#')
	]

-- ADT 
-- We'll be doing lots of lookups and need to grow the grid all directions so a Coord -> State map sounds good
type Grid = S.Set Coord
type Cell = Int
type Coord = (Int, Int, Int, Int)

-- Init grid from parsed data assuming the z and w coordinates are 0
initGrid :: Grid -> [[Cell]] -> Grid
initGrid g = foldr (\(y, row) g -> initRow g y row) g . zipWithIndex

initRow :: Grid -> Int -> [Cell] -> Grid
initRow g y = foldr (\(x, cell) g -> initCell g x y cell) g . zipWithIndex

initCell :: Grid -> Int -> Int -> Cell -> Grid 
initCell g x y 1 = S.insert (x,y,0,0) g 
initCell g x y c = g 

zipWithIndex = zip [0..]

textInput :: IO String
textInput = readFile "data/Day17Data.txt"

main = do
	text <- textInput
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right dat -> do
			let grid = initGrid S.empty dat
			return (part1 grid, part2 grid)
			
countActive :: Grid -> Int
countActive = S.size

neighbourCoords :: Coord -> [Coord]
neighbourCoords (x0,y0,z0,w0) = [
	  (x,y,z,w) 
	  | x <- neighbourRange x0
	  , y <- neighbourRange y0
	  , z <- neighbourRange z0
	  , w <- neighbourRange w0
	  , (x,y,z,w) /= (x0,y0,z0,w0)
	]

neighbourRange x = [x-1, x, x+1]

gridBounds g = S.foldr' mergeBounds b1 g
	where 
		(x0,y0,z0,w0) = (S.elems g)!!0
		b1 = ((x0,x0), (y0,y0), (z0,z0), (w0,w0))
	
mergeBounds (x,y,z,w) (xb, yb, zb, wb) = (mergeBound x xb, mergeBound y yb, mergeBound z zb, mergeBound w wb)


mergeBound v m@(minV, maxV) 
	| v < minV = (v, maxV)
	| v > maxV = (minV, v)
	| otherwise = m
	
growBounds = map4 growBound
growBound (minV, maxV) = (minV - 1, maxV + 1)

map4 f (x,y,z,w) = (f x, f y, f z, f w)

step :: Grid -> Grid
step g = let
	newBounds = growBounds . gridBounds $ g
	g' = foldr (visit g) S.empty (expandCoords newBounds)
	in g'

expandCoords (xb, yb, zb, wb) = do
	x <- bToRange xb
	y <- bToRange yb
	z <- bToRange zb
	w <- bToRange wb
	return (x,y,z,w)

bToRange (minV, maxV) = [minV..maxV]
	
visit currG c newG = let 
	local = currG !? c
	nCoords = neighbourCoords c
	nOn = sum $ map (currG !?) nCoords
	newState = case (local, nOn) of
		(1, 2) -> 1
		(1, 3) -> 1
		(0, 3) -> 1
		otherwise -> 0
	in if newState == 1 then S.insert c newG else newG
		
stepN 0 g = g
stepN n g = stepN (n-1) (step g) 

-- Read grid with default read value of 1 so there is no OOB
map !? k = if k `S.member` map then 1 else 0
			
part2 = countActive . stepN 6
part1 grid = ()
