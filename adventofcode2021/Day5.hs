{-# LANGUAGE ParallelListComp #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Control.Applicative
import Control.Monad(join)
import Data.List
import Data.List.Split(chunksOf)

import qualified Data.Map.Strict as M

-- Boilerplate
dataPath = "data/Day5Data.txt"
aoc_main = H.readAndParse dataPath p_all
solve = aoc_main countOverlaps

-- Input parsing
p_all = endBy p_line newline
p_line = do
	sx <- H.p_int
	char ','
	sy <- H.p_int
	string " -> "
	tx <- H.p_int
	char ','
	ty <- H.p_int
	return ((sx,sy),(tx,ty))

type Coord = (Int, Int)
type CMap = M.Map Coord Int -- Maps coordinates to the number of time they're touches by a line

-- There's only one function for part1 and part2, this "config" constant
-- toggles whether diagonals are ignored or counted
part2 = False

-- Solving function
-- Build a map of all coordinates and the number of times they're touched
-- Then count the number of coordinates with more than one touch
countOverlaps :: [(Coord, Coord)] -> Int
countOverlaps coords = let touches = foldr touch M.empty coords in
  M.foldr (\x s -> s + if x > 1 then 1 else 0) 0 touches 

-- Increment all interpolated coordinates in the map by 1
touch :: (Coord, Coord) -> CMap -> CMap
touch (s,t) m = foldr (\c m -> M.alter incr c m) m (interpolate s t)

incr Nothing = Just 1
incr (Just x) = Just $ x + 1 

-- Return all coordinates between two Coords.
-- If part2 = False, diagonals are ignored
interpolate (sx,sy) (tx,ty) 
  | (sx == tx) || (sy == ty) = [(x,y) | x <- r sx tx, y <- r sy ty]
  | not part2 = [] -- Ignore diagonals
  | part2 = [(x,y) | x <- r' sx tx | y <- r' sy ty] -- Diagonals

r x y 
  | x > y = [y..x]
  | otherwise = [x..y]

r' x y
  | x > y = [x,x-1..y]
  | otherwise = [x..y]
  