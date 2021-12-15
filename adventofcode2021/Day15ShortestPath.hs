{-# LANGUAGE ViewPatterns #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative
import Data.Function ((&), on)

import qualified Data.Map.Strict as M
import qualified Data.Heap as MH

import Data.Char
import Data.List
import Safe
import Debug.Trace

-- Boilerplate
dataPath = "data/Day15Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

main = part1

-- Input parsing
p_all :: Parser [[Int]]
p_all = endBy (fmap digitToInt <$> many1 digit) newline

-- Part1

type Coord = (Int, Int)
type RiskMap = M.Map Coord Int
-- For each coord, the current best-known path from start to that coord and its risk value
type PathMap = M.Map Coord PartialPath
data PartialPath = PartialPath {
	ppRisk :: Int,
	ppHeur :: Int,
	ppPath :: [Coord]}
	deriving (Eq, Show)
type PathHeap = MH.MinHeap PartialPath
	
fScore partial = (ppHeur partial) + (ppRisk partial)
ppHead = head . ppPath
ppCost partial = ppRisk partial + ppHeur partial
	
instance Ord PartialPath where
	compare = compare `on` fScore
	(<=) = (<=) `on` fScore

-- (3.62 secs, 1,043,237,992 bytes)
p1 = solve 1
-- (418.81 secs, 92,371,967,056 bytes)
p2 = solve 5

solve scaleFactor grid = ppRisk $ shortestPath rmScaled (0,0) end where 
	rm = H.gridToMap grid
	rmScaled = scaleGrid scaleFactor rm
	end = maximum . M.keys $ rmScaled
	
scaleGrid :: Int -> RiskMap -> RiskMap
scaleGrid n rm = foldr M.union M.empty . map (scale1 rm) $ [(x,y) | x<-[0..n-1], y<-[0..n-1]]  
scale1 rm (m,n) = M.map dv . M.mapKeys (\(x,y) -> (m*(w+1) + x, n*(h+1) + y)) $ rm where
	dv v = ((v + m + n - 1) `mod` 9) + 1
	(w,h) = maximum . M.keys $ rm

-- This is a weirdly implemented A* that is suboptimal.
-- Among other things
-- * The algorithm should be tracking nodes, not paths.
-- * Risk shouldn't be fully re-computed for each new PartialPath
shortestPath :: RiskMap -> Coord -> Coord -> PartialPath
shortestPath rm start end = go M.empty $ MH.singleton . mkPartial $ [start] where

	go :: PathMap -- Current best known subpaths to visited coords
		-> PathHeap -- Next subpaths to explore
		-> PartialPath -- Optimal path to end
	go pm mh | MH.null mh = error "No path found"
	go pm (MH.view -> Just (partial, mh)) = if isEnd partial then partial else
		case fmap ppRisk . (pm M.!?) . ppHead $ partial of 
			Just knownRisk | knownRisk <= (ppRisk partial) -> go pm mh -- New path isn't better, ignore it
			-- There's either no known subpath or it's worst, replace it
			otherwise -> go pm' mh' where
				pm' = M.insert (ppHead partial) partial pm
				mh' = (MH.union mh (MH.fromList $ nextPaths partial))
				
	nextPaths :: PartialPath -> [PartialPath]
	nextPaths (ppPath -> (c:cs)) = [mkPartial (c':c:cs) | c' <- H.neighbors4 c , M.member c' rm]
	
	heuristic c = (manhattan c end)
	isEnd = (==end) . ppHead
	
	mkPartial cs = PartialPath {
			ppRisk = sum . map (rm M.!) . init $ cs,
			ppHeur = heuristic . head $ cs,
			ppPath = cs
		}
		
manhattan (x,y) (x',y') = abs (x - x') + abs (y - y')
