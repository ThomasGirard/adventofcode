{-# LANGUAGE ViewPatterns #-} 

import qualified Data.Vector as V
import Control.Applicative ((<$>))
import Control.Monad (join)

dat :: IO String
dat = readFile "data/Day12Data.txt"

-- Part 1
parse_all = map (\(d:nn) -> (d, read nn)) . lines 

main = do
	d <- parse_all <$> dat
	return (part1 d, part2 d)

part1 d = let (x,y,_,_) = foldl step1 (0, 0, 1, 0) d in (x,y)

step1 (x, y, dx, dy) (d, n) = case d of
	'N' -> (x, y-n, dx, dy)
	'S' -> (x, y+n, dx, dy)
	'E' -> (x+n, y, dx, dy)
	'W' -> (x-n, y, dx, dy)
	'L' -> (x, y, dx', dy') where (dx', dy') = rl n dx dy
	'R' -> (x, y, dx', dy') where (dx', dy') = rr n dx dy
	'F' -> (x + n*dx, y + n*dy, dx, dy)
	
rl  90  dx  dy = ( dy, -dx)
rl 180  dx  dy = (-dx, -dy)
rl 270  dx  dy = (-dy,  dx)

rr = rl . (`mod` 360) . negate

part2 d = let (x,y,_,_) = foldl step2 (0, 0, 10, -1) d in (x,y)

step2 c@(x, y, dx, dy) op@(d, n) = case d of
	'N' -> (x, y, dx, dy-n)
	'S' -> (x, y, dx, dy+n)
	'E' -> (x, y, dx+n, dy)
	'W' -> (x, y, dx-n, dy)
	otherwise -> step1 c op


ex1 = "F10\nN3\nF7\nR90\nF11"