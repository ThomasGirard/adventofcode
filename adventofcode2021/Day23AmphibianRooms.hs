{-# LANGUAGE ViewPatterns #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Function ((&), on)

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Heap as MH

import Data.Char
import Data.List (sort, nub, transpose)
import Safe
import Debug.Trace
import Data.Maybe
import Data.Either

typeCost 'A' = 1
typeCost 'B' = 10
typeCost 'C' = 100
typeCost 'D' = 1000

-- 01.2.3.4.56
--   A B C D
allLocs = "01A2B3C4D56"

data Loc = 
	 Room Char [Char] -- E.g. (Room 'B' "CD") is Room B where C and D haven't left yet and C is next to leave)
	| Park Char (Maybe Char) -- E.g. (Park '2' Just 'A') is the space between rooms A and B and contains an A.
	deriving (Ord, Eq, Show)
	
type Space = M.Map Char Loc

isDoneSpace :: Space -> Bool
isDoneSpace = all isDoneRoom where
	isDoneRoom (Park _ f) = isNothing f
	isDoneRoom (Room r frogs) = all (==r) frogs

startLocs :: [String] -> Space
startLocs input = M.fromList $
	   [(p, Park p Nothing) | p <- ['0'..'6']]
	++ [(r, Room r frogs) | (r, frogs) <- zip "ABCD" input]

--------------
-- MOVEMENT
--------------
nextSpaces :: Space -> [(Int, Space)]
nextSpaces s = catMaybes [move s f t | f <- allLocs, t <- allLocs, isMixedMove f t] where
	-- Only allow moves from Room to Park or Park to Room
	-- (Room - Room is allowed by the rules but is functionally the same as 2 separate moves)
	isMixedMove a b | isDigit a && isLetter b = True
	isMixedMove b a | isDigit a && isLetter b = True
	isMixedMove _ _ = False

move :: Space -> Char -> Char -> Maybe (Int, Space)
move s fromI toI = let (from, to) = (s M.! fromI, s M.! toI) in do
	(c, from') <- empty from
	to' <- fill to c
	dist <- checkPath s from to
	return $ (dist * typeCost c, 
		  s
		& M.insert (name from') from' 
		& M.insert (name to') to')
	
-- Check that all locations between two given locations excluded are free.
-- Return Just the length of the path if they're free.
checkPath :: Space -> Loc -> Loc -> Maybe Int
checkPath s f t = case locsBetween s f t of
	locs | all isFree locs -> Just $ pathLength (name f) (name t)
	otherwise -> Nothing
	where
	isFree (Room _ _) = error "Should not traverse a room"
	isFree (Park _ Nothing) = True
	isFree _ = False

locsBetween :: Space -> Loc -> Loc -> [Loc]
locsBetween s f t = map (s M.!) range where
	range = locsBetweenI (name f) (name t)
	
locsBetweenI :: Char -> Char -> [Char]
locsBetweenI f t = filter isDigit . map (allLocs!!) $ range where
	fi = elemIndexJust f allLocs
	ti = elemIndexJust t allLocs
	range = if fi < ti 
		then [fi+1..ti-1] 
		else [fi-1,fi-2..ti+1]
		
name (Park p _) = p
name (Room r _) = r

empty :: Loc -> Maybe (Char, Loc)
-- Can leave a full park
empty (Park p (Just f)) = Just (f, (Park p Nothing))
-- Can leave a room iff there's at least one "incorrect" element in.
empty (Room r (f:fs)) | any (/=r) (f:fs) = Just (f, (Room r fs))
empty _ = Nothing

fill :: Loc -> Char -> Maybe Loc
-- Can enter an empty park
fill (Park p Nothing) f = Just $ (Park p (Just f))
-- Can enter a room if it's only correct elements
fill (Room r fs) f | all (==r) (f:fs) = Just $ (Room r (f:fs))
fill _ _ = Nothing


-- 01.2.3.4.56
--   A B C D
pathLength f t | f < t = pathLength t f
pathLength 'A' '0' = 3
pathLength 'A' '1' = 2
pathLength 'A' '2' = 2
pathLength 'A' '3' = 4
pathLength 'A' '4' = 6
pathLength 'A' '5' = 8
pathLength 'A' '6' = 9

pathLength 'B' '0' = 5
pathLength 'B' '1' = 4
pathLength 'B' '2' = 2
pathLength 'B' '3' = 2
pathLength 'B' '4' = 4
pathLength 'B' '5' = 6
pathLength 'B' '6' = 7

pathLength 'C' '0' = 7
pathLength 'C' '1' = 6
pathLength 'C' '2' = 4
pathLength 'C' '3' = 2
pathLength 'C' '4' = 2
pathLength 'C' '5' = 4
pathLength 'C' '6' = 5

pathLength 'D' '0' = 9
pathLength 'D' '1' = 8
pathLength 'D' '2' = 6
pathLength 'D' '3' = 4
pathLength 'D' '4' = 2
pathLength 'D' '5' = 2
pathLength 'D' '6' = 3

---------------
-- Tree
---------------

type Cost = Int
type Node = (Cost, Space)

step :: [Node] -> [Node]
step = concatMap nextNodes

nextNodes :: Node -> [Node]
nextNodes (c, s) = map (\(c', s') -> (c+c', s')) (nextSpaces s)

-- Map of Spaces to their lowest known cost
type NodeMap = M.Map Space Cost -- Current best known value to each Coord
type NodeHeap = MH.MinHeap Node

shortestPath :: Space -> Node
shortestPath start = go M.empty . MH.singleton $ (0, start) where
	go :: NodeMap -- Current best known Node for each Coord
		-> NodeHeap -- Next Nodes to explore
		-> Node -- Optimal path to end
	go nm nh | MH.null nh = error "No path found"
	go nm (MH.view -> Just (node@(cost, space), nh)) = if isDone node then node else
		case nm M.!? space of
			-- New node isn't better, ignore it
			Just knownCost | knownCost <= cost -> go nm nh 
			-- Space hasn't been visited yet, or the new path has lower cost, use it
			otherwise -> go nm' nh' where
				nm' = M.insert space cost nm
				nh' = (MH.union nh (MH.fromList $ nextNodes node))
	isDone = isDoneSpace . snd
				
--------------------
-- DISPLAY
--------------------

display :: Node -> IO ()
display (n, s) = do
	putStrLn $ "$" ++ (show n) ++ ": "
	display' s

display' :: Space -> IO ()
display' = mapM_ putStrLn . toString
	
toString :: Space -> [String]
toString s = [toChar s p | p <- "01 2 3 4 56"] : 
	transpose [toStringRoom s r | r <- "  A B C D  "]
	
toStringRoom s r = case s M.!? r of 
	(Just (Room r fs)) -> reverse . take 4 . reverse $ "    " ++ fs
	_ -> "    "
	
toChar s ' ' = ' '
toChar s k = case s M.!? k of
	Just (Park _ (Just x)) -> x
	_ -> '.'
	
------------------------
-- TESTING / DEBUG
------------------------

inputTest1 = ["BA","CD","BC","DA"] -- KO got 12723/10510 (>) expected 12521/10308 (+1111 + 1102)
inputTest2 = ["BDDA","CCBD","BBAC","DACA"] -- KO, expected
input1 = ["AC","DC","AD","BB"] -- OK, 13495
input2 = ["ADDC","DCBC","ABAD","BACB"] -- OK 53767

solve input = outOffset + inOffset + pathCost where
	pathCost = fst . shortestPath . startLocs $ input
	outOffset = let n = (length . transpose $ input) - 1 in 1111 * (n * (n+1)) `div` 2
	inOffset = sum [m * 10^((ord c) - ord 'A') | (m,row) <- zip [0..] (transpose input), c <- row]

part1 = solve input1
part2 = solve input2
