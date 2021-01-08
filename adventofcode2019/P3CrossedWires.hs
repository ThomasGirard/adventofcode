{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)

import Data.Function (on)
import Control.Applicative (pure, (*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Data.Either (lefts, rights)

import Debug.Trace (trace)

textInput :: IO String
textInput = readFile "data/3.txt"

main = do
	text <- textInput
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right exprs -> return exprs

input = unsafePerformIO $ main

p_all = do
	w1 <- p_wire
	newline
	w2 <- p_wire
	newline
	return (w1, w2)

p_wire = sepBy p_segment (char ',') 

p_segment = do
	dir <- satisfy (`elem` "LUDR")
	dist <- p_int
	return (dir, dist)

p_int :: Parser Int
p_int = read <$> many1 digit

type Cost = Int
type RelSeg = (Char, Int)
type AbsSeg = (Coord, Coord)
type RelPath = [RelSeg]
type Coord = (Cost, Int, Int)
type AbsPath = [AbsSeg]

-- Two choices of algorithm : expand paths explicitely 
-- (all coordinates visited) and look for intersections (might be slower/less memory efficient)
-- Or match segments pairwise to find intersections. The problem is segments are relative...
-- How long are those wires, let's see (about 150'000 each)
-- Too long to fully expand and find intersections (n^2) so let's do a conversion from relative to absolute for segments

relToAbsPath :: Coord -> RelPath -> AbsPath
relToAbsPath _ [] = []
relToAbsPath origin (seg:path) = let seg'@(f,t) = relToAbsSeg origin seg
	in seg':(relToAbsPath t path)

relToAbsSeg (c0, x0, y0) seg = ((c0, x0, y0), (c0+dc, x0+dx, y0+dy)) where
	(dc, dx, dy) = relSegToDelta seg

-- Coordinate system is x increases to the right, y increases to the bottom
relSegToDelta ('U', n) = (n,  0, -n)
relSegToDelta ('D', n) = (n,  0,  n)
relSegToDelta ('L', n) = (n, -n,  0)
relSegToDelta ('R', n) = (n,  n,  0)

right (Right x) = x

intersectPaths p q = catMaybes [intersectSegs' pp pq | pp <- map distillSeg p, pq <- map distillSeg q]

intersections (p, q) = filter (\(_,x,y) -> (x,y) /= (0,0)) $ intersectPaths (relToAbsPath (0,0,0) p) (relToAbsPath (0,0,0) q)

part1 = minimum . (map manhattan) . intersections
part2 = minimum . (map (\(c,x,y) -> c)) . intersections

manhattan (_,x,y) = (abs x) + (abs y)

-- For two perpendicular segments they intersect if the fixed-part of one is within the opposite part of the other and vice-versa
-- A horizontal segment has a fixed y and that y must be within the other segment's x
intersectSegs' (cs, sh, sf, sb) (ct, th, tf, tb) = case (sh == th, inBounds sf tb, inBounds tf sb) of
	(False, Just ds, Just dt) -> Just (cs + ct + ds + dt, sf, tf)
	otherwise -> Nothing

inBounds :: Int -> (Int, Int) -> Maybe Int
inBounds x (y0, y1) 
	| x >= y0 && x <= y1 = Just . abs $ x - y0
	| x >= y1 && x <= y0 = Just . abs $ x - y0
	| otherwise = Nothing
		
type SegId = (
	Int, -- Starting cost
	Bool, -- True iff the segment is vertical
	Int, -- The fixed x or y value of the segment
	(Int, Int) -- The bounds of the non-fixed value of the segment, start to end
	)
	
distillSeg :: AbsSeg -> SegId
distillSeg ((c0, x0, y0), (c1, x1, y1)) 
	| x0 == x1 = (c0, True , x0, (y0, y1))
	| y0 == y1 = (c0, False, y0, (x0, x1))
	
	

			 
-- Debug / Testing

ex0 = right $ parse p_all "" $ "R8,U5,L5,D3\nU7,R6,D4,L4\n"

ex1str = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83\n"
ex1 = right $ parse p_all "" ex1str


-- Unused
{-

-- Hope it's fine to assume at most one intersection based on the problem statement (i.e. not overlapping non-//)
-- There is a more efficient version of this that doesn't use expansion
intersect_Segs :: AbsSeg -> AbsSeg -> Maybe Coord
intersect_Segs x y = case (intersect `on` expand) x y of 
	[] -> Nothing
	[x] -> Just x
	_ -> error "Overlapping segments!"

	

expand :: AbsSeg -> [Coord]
expand ((x0, y0), (x1, y1)) = [(x,y) | x <- absRange x0 x1, y <- absRange y0 y1]

absRange :: Int -> Int -> [Int]
absRange x y | x <= y    = [x..y]
			 | otherwise = [y..x]
			 
-}
