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
dataPath = "data/Day22Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

data Instr = On Cuboid | Off Cuboid deriving (Ord, Eq, Show)
type Rng = (Int,Int)
type Cuboid = [Rng] -- Always three coords

-- Input parsing
p_all :: Parser [Instr]
p_all = endBy p_instr newline

p_instr = do
	onOff <- choice [
		On <$ (try $ string "on"), 
		Off <$ (string "off")
		]
	char ' '
	x <- p_range 'x'
	char ','
	y <- p_range 'y'
	char ','
	z <- p_range 'z'
	return $ onOff [x,y,z]
	
p_range k = do
	char k
	char '='
	from <- H.p_sint
	string ".."
	to <- H.p_sint
	return (from, to)

-- Part 1
-- Likely for part2 it won't make sense to enumerate cubes, but let's do that for now
p1 = S.size . foldr execInstr S.empty . reverse . filter isInBoundsInstr

execInstr (On  xyz) set = S.union set (toSet xyz)
execInstr (Off xyz) set = S.difference set (toSet xyz)

toSet [rx, ry, rz] = S.fromList [(x,y,z) 
	| x <- expandRange rx, y <- expandRange ry, z <- expandRange rz]
expandRange (a,b) = [a..b]

isInBoundsInstr (On xyz) = isInBoundsCuboid xyz
isInBoundsInstr (Off xyz) = isInBoundsCuboid xyz
isInBoundsCuboid ranges = all isInBoundsRng ranges
isInBoundsRng (f,t) = isInBoundsC f && isInBoundsC t
isInBoundsC c = c <= 50 && c >= -50

-- Part 2
-- Cuboid arithmetic
-- We're going to define cuboid union and substraction methods
-- such that we can apply these methods until we're left with a list of
-- completely disjoint cuboids.
-- Then, rather than enumerate points we can just sum the volumes.

-- UNION
-- The union of two cuboids A B depends on how they overlap : 
-- A and B are disjoint, then the union is still the disjoint A and B.
-- A is fully included in B, then the union is B (or vice versa)
-- A and B partially overlap. In each axis, the ranges define up to 3 internal
-- subranges (e.g. on x : Ax, Ax inter Bx, Bx). Each of these subranges on 
-- each axis forms up to 3*3*3 = 27 new disjoint cuboids (some will not belong to either A or B and can be discarded).
-- For example if the cuboids intersect such that only one vertex of each is in the other, they form 15 new sub cuboids, 7 eighths of each parent and one at their intersection).
-- Also it turns out that this was completely unnecessary, see makeDisjoint below.
union :: Cuboid -> Cuboid -> [Cuboid]
union a b | a `isDisjoint` b = [a,b]
union a b | a `isSubsetOf` b = [b]
union a b | b `isSubsetOf` a = [a]
union a b = newCuboids where
	[bx, by, bz] = zipWith newBounds a b
	sub27 = [[x,y,z] | x <- bx, y <- by, z <- bz]
	newCuboids = filter isProper sub27
	isProper c = (c `isSubsetOf` a) || (c `isSubsetOf` b)
	
-- Note the new bounds are disjoint
newBounds (f,t) (f', t') = let [a,b,c,d] = sort [f,t,f',t'] in
	[(a,b-1),(b,c),(c+1,d)]

axyz `isSubsetOf` bxyz = and $ zipWith isSubsetOfRng axyz bxyz
(f,t) `isSubsetOfRng` (f',t') = (f' <= f) && (t <= t')

axyz `isDisjoint` bxyz = or $ zipWith isDisjointRng axyz bxyz
(f,t) `isDisjointRng` (f',t') = (t < f') || (f > t')

-- SUBSTRACTION
-- For substraction similar rules apply : 
-- A - B is A if A and B are disjoint
-- A - B is 0 if A is a subset of B
-- A - B in the general case uses a similar algorithm to A + B in the general case
-- but only fewer of the subdivided cuboids will be retained.
-- Thankfully this is almost the same as above so my time wasn't completely wasted.
subst :: Cuboid -> Cuboid -> [Cuboid]
subst a b | a `isDisjoint` b = [a]
subst a b | a `isSubsetOf` b = []
subst a b = newCuboids where
	[bx, by, bz] = zipWith newBounds a b
	sub27 = [[x,y,z] | x <- bx, y <- by, z <- bz]
	newCuboids = filter isProper sub27
	isProper c = (c `isSubsetOf` a) && (c `isDisjoint` b)

cuboidSize :: Cuboid -> Integer
cuboidSize = product . map rangeSize 
rangeSize (f,t) = (fromIntegral $ t-f) + 1

ccsize :: [Cuboid] -> Integer
ccsize = sum . map cuboidSize

p2 = sum . map cuboidSize . foldl makeDisjoint [] where
	

-- Given a list of disjoint cuboids, add or remove a new one
makeDisjoint :: [Cuboid] -> Instr -> [Cuboid]
-- Turns out the union function we wrote above is really cute but also completely useless.
-- Using union would lead to (non-disjoint) fractions of the new cube being repeatedly added to the list.
-- The correct way is to substract the new cube from all existing ones
-- and then add it untouched to the list. 
makeDisjoint disj (On  b) = b : concatMap (\a -> a `subst` b) disj
-- To turn off, substract the new cube from all existing cubes.
makeDisjoint disj (Off b) =     concatMap (\a -> a `subst` b) disj
-- What I naively wrote initially which is both incorrect and will generate
-- a ton of overlapping cubes cubes : 
-- makeDisjoint disj (On  b) = concatMap (union b) disj


-- Some testing
testA = [(0,1),(0,1),(0,1)] -- size 8
testB = [(1,3),(1,3),(1,3)] -- size 27
testC = [(2,2),(2,2),(2,2)] -- size 1, fully contained in B

-- A and B intersect only over (1,1,1) so 
-- A union B defines 15 cuboids of total size 27+8-1 = 34
testU1 = (==34) . ccsize $ union testA testB
-- union is commutative
testU2 = (==34) . ccsize $ union testB testA
testU3 = (==27) . ccsize $ union testB testB

testS1 = (==7) . ccsize $ subst testA testB
testS2 = (==26) . ccsize $ subst testB testA
testS3 = (==0) . ccsize $ subst testB testB
testS4 = (==26) . ccsize $ subst testB testC
testS5 = (==0) . ccsize $ subst testC testB
