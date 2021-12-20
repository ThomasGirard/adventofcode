{-# LANGUAGE ViewPatterns #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Function ((&), on)

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Data.Char
import Data.List
import Safe
import Debug.Trace
import Data.Maybe

-- Boilerplate
dataPath = "data/Day19Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all :: Parser [Scanner]
p_all = endBy p_scanner newline

p_scanner = do
	string "--- scanner "
	scannerNo <- H.p_int 
	string " ---"
	newline
	measures <- endBy1 p_measure newline
	return (scannerNo, measures)

p_measure = sepBy1 H.p_sint (char ',') 

type Measure = [Int] 
type Measures = [Measure]
type Scanner = (Int, Measures)

-- Dealing with orientations of Measures
type Orientation = (Facing, Rotation)
data Facing = Xp | Xn | Yp | Yn | Zp | Zn deriving (Eq, Ord, Show)
data Rotation = Rot0 | Rot90 | Rot180 | Rot270 deriving (Eq, Ord, Show)

allOrientations = [
	(f, r) 
	| f <- [Xp, Xn, Yp, Yn, Zp, Zn]
	, r <- [Rot0, Rot90, Rot180, Rot270]
	]
orient :: Measures -> Orientation -> Measures
orient ms or = map (orient1 or) ms where
	orient1 (Xp, Rot0  ) [x,y,z] = [ x, y, z]
	orient1 (Xp, Rot90 ) [x,y,z] = [ x, z,-y]
	orient1 (Xp, Rot180) [x,y,z] = [ x,-y,-z]
	orient1 (Xp, Rot270) [x,y,z] = [ x,-z, y]
	orient1 (Xn, Rot0  ) [x,y,z] = [-x, y,-z]
	orient1 (Xn, Rot90 ) [x,y,z] = [-x,-z,-y]
	orient1 (Xn, Rot180) [x,y,z] = [-x,-y, z]
	orient1 (Xn, Rot270) [x,y,z] = [-x, z, y]
	orient1 (Yp, Rot0  ) [x,y,z] = [ y,-x, z]
	orient1 (Yp, Rot90 ) [x,y,z] = [ y, z, x]
	orient1 (Yp, Rot180) [x,y,z] = [ y, x,-z]
	orient1 (Yp, Rot270) [x,y,z] = [ y,-z,-x]
	orient1 (Yn, Rot0  ) [x,y,z] = [-y, x, z]
	orient1 (Yn, Rot90 ) [x,y,z] = [-y, z,-x]
	orient1 (Yn, Rot180) [x,y,z] = [-y,-x,-z]
	orient1 (Yn, Rot270) [x,y,z] = [-y,-z, x]
	orient1 (Zp, Rot0  ) [x,y,z] = [ z, y,-x]
	orient1 (Zp, Rot90 ) [x,y,z] = [ z,-x,-y]
	orient1 (Zp, Rot180) [x,y,z] = [ z,-y, x]
	orient1 (Zp, Rot270) [x,y,z] = [ z, x, y]
	orient1 (Zn, Rot0  ) [x,y,z] = [-z, y, x]
	orient1 (Zn, Rot90 ) [x,y,z] = [-z, x,-y]
	orient1 (Zn, Rot180) [x,y,z] = [-z,-y,-x]
	orient1 (Zn, Rot270) [x,y,z] = [-z,-x, y]

-- An assembled scanner with 
-- * Its number (same as the source scanner)
-- * Translated and oriented coordinates, aligned to the scanner at (0,0)
-- * The translation vector from (0,0)
type Assembled = (Int, Measures, [Int])

-- Base : 
-- Improved : (125.30 secs, 13,571,682,864 bytes)
p1 :: [Scanner] -> Int
p1 scanners = 
	assembleAll scanners
	& concatMap (\(_,m,_) -> m)
	& sort
	& nub
	& length

assembleAll (s0:scans) = assemble [initAssembled s0] scans where
	initAssembled (n, m) = (n, m, [0,0,0])

p2 scanners = maximum [manhattanDist s t | s <- sPositions, t <- sPositions] where
	sPositions = map (\(_,_,v) -> v) . assembleAll $ scanners
	manhattanDist x y = sum $ zipWith (\a b -> abs (a-b)) x y

assemble :: [Assembled] -> [Scanner] -> [Assembled]
assemble done [] = done
assemble done (s:next) = case assembleNext done s of
	Just a' -> assemble (a':done) next
	Nothing -> assemble done (next ++ [s])

-- Try matching a Scanner with any already Assembled scan.
-- Multiple matches may occur, any of them can be used.
assembleNext :: [Assembled] -> Scanner -> Maybe Assembled	
assembleNext ass s = listToMaybe $ mapMaybe (flip orientAndMatch $ s) ass

-- Try matching a Scanner with an already Assembled scan.
-- This and children functions are the bottleneck
-- measuresMatch is re-computed for every attempted match, many of which are needlessly repeated often.
orientAndMatch :: Assembled -> Scanner -> Maybe Assembled
orientAndMatch (_, m0, _) (s1name, m1) 
	= listToMaybe . mapMaybe tryOrient . orientationsOf $ m1 
	where
	tryOrient mOriented = case measuresMatch m0 mOriented of
		Nothing -> Nothing
		Just p -> Just (s1name, translated, v) where
			(v, translated) = translate p m0 mOriented 
		
-- All the possible orientations of the given Measures.
orientationsOf :: Measures -> [Measures]
orientationsOf ms = map (orient ms) allOrientations

-- Given two Measures, return True iff there is 
-- at least 12 overlapping coordinates between them (ignoring translations).
-- To do so : 
-- 1) For each point in each set, compute the list of distances between this point and others
-- 	  in the set. 
-- 2) Compare these vector lists from one set to the other. Any list that matches at least 
--    12 items in the other scan is a valid point.
-- 3) If there are 12 or more such lists, then a clique of 12 matching points is found, and 
--    measurements are a match.
-- One of the matching points is returned on match.
-- This works because the vectors are translation independant.
type Point = [Int]
type DistVec = [[Int]]

measuresMatch :: Measures -> Measures -> Maybe Point
measuresMatch s0 s1 = if M.size matchingPoints >= 12 
	then Just . fst . M.elemAt 0 $ matchingPoints
	else Nothing
	where
	refPoints = map snd $ distVectors s0
	canPoints = M.fromList $ distVectors s1
	matchingPoints = filterPoints refPoints canPoints
	
	distVectors :: Measures -> [(Point, DistVec)]
	distVectors ms = [ (m1, [vectorDiff m1 m2 | m2 <- ms]) | m1 <- ms]
	
	filterPoints :: [DistVec] -> M.Map Point DistVec -> M.Map Point DistVec
	filterPoints refVecs canVecs = 
		M.filter (\distVec -> any (distancesMatch distVec) refVecs) canVecs where
		distancesMatch :: DistVec -> DistVec -> Bool
		distancesMatch pointCan pointRef = (>=12) . length $ intersect pointCan pointRef
	
-- Given reference measures m0 and m1, oriented but not translated,
-- translate the measures m1 and return them along with the translation vector.
-- refPoint is given as a point in m1 that is guaranteed to be matched in m0.
translate :: Point -> Measures -> Measures -> ([Int], Measures)
translate refPoint m0 m1 = 
	head 
	. filter (isGoodTranslation . snd)
	. map (\v -> (v, translateM m1 v))
	$ [vectorDiff p0 refPoint | p0 <- m0] where
		isGoodTranslation = (>=12) . length . (intersect m0)
		translateM m v = map (zipWith (+) v) m
		
		
vectorDiff :: [Int] -> [Int] -> [Int]
vectorDiff = zipWith (-)