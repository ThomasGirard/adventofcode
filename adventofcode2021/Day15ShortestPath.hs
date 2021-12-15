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

-- Input parsing
p_all :: Parser [[Int]]
p_all = endBy (fmap digitToInt <$> many1 digit) newline

-- Part1

type Coord = (Int, Int)
data Node = Node {
	nRisk :: Int,
	nCoord :: Coord
	} deriving (Eq, Show)
type RiskMap = M.Map Coord Int
type NodeMap = M.Map Coord Node -- Current best known path to each Coord
type NodeHeap = MH.MinHeap Node
	
instance Ord Node where
	compare = compare `on` nRisk
	(<=) = (<=) `on` nRisk

p1 = solve 1
-- Old (418.81 secs, 92,371,967,056 bytes)
-- New ( 22.30 secs,  5,407,613,872 bytes)
p2 = solve 5

solve scaleFactor grid = nRisk $ shortestPath rmScaled (0,0) end where 
	rm = H.gridToMap grid
	rmScaled = scaleGrid scaleFactor rm
	end = maximum . M.keys $ rmScaled
	
scaleGrid :: Int -> RiskMap -> RiskMap
scaleGrid n rm = foldr M.union M.empty . map (transposeGrid rm) $ [(x,y) | x<-[0..n-1], y<-[0..n-1]]
transposeGrid rm (m,n) = M.map updateVal . M.mapKeys (\(x,y) -> (m*(w+1) + x, n*(h+1) + y)) $ rm where
	(w,h) = maximum . M.keys $ rm
	updateVal v = ((v + m + n - 1) `mod` 9) + 1

-- Djikstra
-- An old version used A* but the only reasonable heuristic in this case doesn't improve performance.
shortestPath :: RiskMap -> Coord -> Coord -> Node
shortestPath rm start end = go M.empty . MH.singleton . startNode $ start where

	go :: NodeMap -- Current best known Node for each Coord
		-> NodeHeap -- Next Nodes to explore
		-> Node -- Optimal path to end
	go nm nh | MH.null nh = error "No path found"
	go nm (MH.view -> Just (node, nh)) = if isEnd node then node else
		case fmap nRisk . (nm M.!?) . nCoord $ node of
			-- New node isn't better, ignore it
			Just knownRisk | knownRisk <= (nRisk node) -> go nm nh 
			-- Coordinate hasn't been visited yet, or the new Node has lower risk, use it
			otherwise -> go nm' nh' where
				nm' = M.insert (nCoord node) node nm
				nh' = (MH.union nh (MH.fromList $ nextNodes node))
				
	nextNodes :: Node -> [Node]
	nextNodes node = [mkNode risk next | next <- H.neighbors4 curr , M.member next rm] where
		risk = nRisk node
		curr = nCoord node
	
	isEnd = (==end) . nCoord
	
	startNode coord = Node {
		nRisk = 0,
		nCoord = coord
		}
		
	mkNode r curr = Node {
		nRisk = r + (rm M.! curr),
		nCoord = curr
		}
