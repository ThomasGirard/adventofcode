{-# LANGUAGE ViewPatterns #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Function ((&), on)

import qualified Data.Map.Strict as M

import Data.Char
import Data.List
import Safe
import Debug.Trace
import Data.Maybe

-- Input
{--}
xMin = 209
xMax = 238
yMin = -86
yMax = -59
{--}

-- Test
{-
xMin = 20
xMax = 30
yMin = -10
yMax = -5
-}

inTarget (x,y) = and [
	x >= xMin,
	x <= xMax,
	y >= yMin,
	y <= yMax
	]

type Coord = (Int, Int)
	
-- Solution 1, brute force
	
part1 = maximum . map snd . snd . H.maximumOn (snd.fst) . concatMap stepsGivenVX $ xRange where
part2 = length . concatMap stepsGivenVX $ xRange

-- Imagine we extend the target area infinitely vertically.
-- There is an upper bound after which no X value will ever touch this area.
-- X=1 is a minimum as well by definition.
xRange = [1..xMax] 
yRange = [1000,999..(-1000)]

-- Given an X value, generate the coordinates for all possible Ys 
-- where there is at least one intersection
-- Assume an Y upper bound of 1000 for now
stepsGivenVX :: Int -> [((Int,Int), [Coord])]
stepsGivenVX vx = mapMaybe (\vy -> steps (vx, vy)) yRange where
	steps (vx, vy) = let 
		coords = map snd . takeWhile canReach . iterate step $ ((vx,vy),(0,0))
		step ((vx, vy), (x,y)) = ((max 0 (vx-1), vy-1), (x+vx, y+vy))
		in
		if any inTarget coords 
		then Just ((vx,vy), coords)
		else Nothing

canReach ((vx,vy),(x,y)) 
	| x > xMax = False
	| vy<0 && y < yMin = False
	| otherwise = True
