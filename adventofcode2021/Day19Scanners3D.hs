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

import System.IO.Unsafe

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
	measures <- endBy1 (try p_measure) newline
	return (scannerNo, measures)

p_measure = sepBy1 H.p_sint (char ',') 

type Measure = [Int] 
type Measures = [Measure]
type Scanner = (Int, [Measure])
sID (s, _) = s

type Orientation = (Facing, Rotation)
data Facing = Xp | Xn | Yp | Yn | Zp | Zn deriving (Eq, Ord, Show)
data Rotation = Rot0 | Rot90 | Rot180 | Rot270 deriving (Eq, Ord, Show)

-- Dealing with orientations of Measures
allOrientations = [(f, r) | f <- [Xp, Xn, Yp, Yn, Zp, Zn], r <- [Rot0, Rot90, Rot180, Rot270]]

orientationsOf :: Measures -> [(Orientation, Measures)]
orientationsOf ms = map (\o -> (o, orient ms o)) allOrientations

o1 = nub . sort $ map ((flip orient1) [1,2,3]) allOrientations

inverse :: Orientation -> Orientation
inverse o = head $ filter (\o' -> (==[1,2,3]) . orient1 o' . orient1 o $ [1,2,3]) allOrientations

orient ms or = map (orient1 or) ms
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
-- Its parent, and the Orientation relative to its parent
-- The Scanner instance is aligned to s0
type Assembled = (Scanner, Maybe Int, Orientation, [Int])

p1 :: [Scanner] -> Int
p1 (s0:scans) = length . nub . sort . concatMap (\((_,m),_,_,_) -> m) $ assemble [initAssembled s0] scans

p2 (s0:scans) = let
	scanPos = map (\(_,_,_,v) -> v) $ assemble [initAssembled s0] scans
	in maximum [manhattanDist s t | s <- scanPos, t <- scanPos]

manhattanDist :: [Int] -> [Int] -> Int	
manhattanDist x y = sum $ zipWith (\a b -> abs (a-b)) x y

initAssembled s@(_, m) = (s, Nothing, (Xp, Rot0), [0,0,0])

assemble :: [Assembled] -> [Scanner] -> [Assembled]
assemble done [] = done
assemble done ((s@(sName,_)):next) = case assembleNext done s of
	Just a' -> assemble (a':done) next
	Nothing -> assemble done (next ++ [s])

assembleNext :: [Assembled] -> Scanner -> Maybe Assembled	
assembleNext refs s = listToMaybe $ mapMaybe (\(sRef, _, _, _) -> orientAndMatch sRef s) refs
	
orientAndMatch :: Scanner -> Scanner -> Maybe Assembled
orientAndMatch (s0name, m0) (s1name, m1) 
	= listToMaybe . mapMaybe tryOrient . orientationsOf $ m1 where
		tryOrient (o, mOriented) = case matchCoords m0 mOriented of
			Just _ -> Just ((s0name, translated), Just s0name, o, v) where
				(v, translated) = translate m0 mOriented
			Nothing -> Nothing
		
-- Given reference measures (m0) and m1, aligned but not translated
-- translate the measures m1 and return them
translate :: Measures -> Measures -> ([Int], Measures)
translate m0 m1 = 
	head 
	. filter (isGoodTranslation . snd)
	. map (\v -> (v, translateM m1 v))
	$ [dist p0 p1 | p0 <- sort m0, p1 <- sort m1] where
		isGoodTranslation = (>=12) . length . (intersect m0)

translateM m [dx,dy,dz] = map (\[x,y,z] -> [x+dx, y+dy, z+dz]) m
		
matchCoords :: Measures -> Measures -> Maybe Int
matchCoords s0 s1 = let
	vect0 = distVectors s0
	vect1 = distVectors s1
	vect1' = keep12 vect0 vect1
	in if M.size vect1' >= 12 
		then Just (length s1 - M.size vect1')
		else Nothing
		
type Dist = [Int]

-- Keep only the vectors of the second map that match at least 12 distances in one or more vectors of the first map
keep12 ref v1 = M.filter vHasMatch v1 where
	vHasMatch candidate = any (vvMatches candidate) (M.elems ref)
vvMatches :: (M.Map Measure Dist) -> (M.Map Measure Dist) -> Bool
vvMatches candidate target = (>=12) . length $ intersect (M.elems candidate) (M.elems target)

distVectors ms = M.fromList [ (m1, M.fromList [(m2, dist m1 m2) | m2 <- ms]) | m1 <- ms]
	
dist = zipWith (-)
