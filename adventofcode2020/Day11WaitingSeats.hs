{-# LANGUAGE ViewPatterns #-} 

import qualified Data.Vector as V
import Control.Applicative ((<$>))
import Control.Monad (join)

dat :: IO String
dat = readFile "data/Day11Data.txt"

-- Part 1
parse_all = V.fromList . map V.fromList . lines

main = do
	d <- parse_all <$> dat
	return (part1 d, part2 d)

part1 g = let 
	ss = steps g stepSeat1
	stable = fst . head . filter (uncurry (==)) . zip ss $ (drop 1 ss) 
	in countOccupied stable

type Grid = V.Vector (V.Vector Char)
type StepFun = Grid -> Char -> Int -> Int -> Char

countOccupied :: Grid -> Int
countOccupied = V.sum . fmap (V.length . V.filter (== '#'))

steps :: Grid -> StepFun -> [Grid]
steps g sf = let g' = step g sf in g':(steps g' sf)

step :: Grid -> StepFun -> Grid
step g sf = do 
	(r, row) <- zwi g
	return $ do 
		(c, seat) <- zwi row
		return $ sf g seat r c

stepSeat1 :: StepFun
stepSeat1 _ '.' _ _ = '.'
stepSeat1 g s r c = case (s, occupiedNeighbors1 g r c) of 
	('L', 0) -> '#'
	('#', n) | n >= 4 -> 'L'
	(s, _) -> s

occupiedNeighbors1 g r c = sum [1 
	| dr <- [-1..1]
	, dc <- [-1..1]
	, (dr, dc) /= (0,0)
	, (atDef (atDef g (r+dr) V.empty) (c+dc) '.') == '#'
	]

part2 g = let 
	ss = steps g stepSeat2
	stable = fst . head . filter (uncurry (==)) . zip ss $ (drop 1 ss) 
	in countOccupied stable

stepSeat2 :: StepFun
stepSeat2 _ '.' _ _ = '.'
stepSeat2 g s r c = case (s, occupiedNeighbors2 g r c) of 
	('L', 0) -> '#'
	('#', n) | n >= 5 -> 'L'
	(s, _) -> s
	
occupiedNeighbors2 g r c = sum . fmap (lookAt g r c) $ [(-1,-1), (-1,0), (-1,1), (0, -1), (0,1), (1, -1), (1, 0), (1,1)]

lookAt g r c (dr, dc) = let
	r' = r+dr
	c' = c+dc
	s' = (atDef (atDef g r' V.empty) c' 'L')
	in case s' of 
		'L' -> 0
		'#' -> 1
		'.' -> lookAt g r' c' (dr, dc)


-- Util
		
zwi = V.zip (V.fromList [0..99])

atDef g c d
	| c < 0 = d
	| c >= V.length g = d
	| otherwise = g V.! c
 
mToV :: [[a]] -> V.Vector (V.Vector a)
mToV = V.fromList . map V.fromList
 
ex1 = mToV $ [
	"L.LL.LL.LL",
	"LLLLLLL.LL",
	"L.L.L..L..",
	"LLLL.LL.LL",
	"L.LL.LL.LL",
	"L.LLLLL.LL",
	"..L.L.....",
	"LLLLLLLLLL",
	"L.LLLLLL.L",
	"L.LLLLL.LL"
	]
	