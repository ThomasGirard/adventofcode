{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import qualified Data.List.Ordered as OL
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)

import Control.Applicative (pure, (*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Data.Either (lefts, rights)

import Debug.Trace (trace)

-- ADT & Parsing
type Parsed = [PTile]
type PTile = (Int, [[Int]])

p_all :: Parser Parsed
p_all = do
	tiles <- many1 p_tile
	eof
	return tiles
	
p_tile = do
	string "Tile "
	no <- read <$> many1 digit
	char ':'
	newline
	pixels <- count 10 p_row
	newline
	return (no, pixels)
	
p_row = do
	pixels <- count 10 p_pixel 
	newline
	return pixels

p_pixel = (1 <$ char '#') <|> (0 <$ char '.')

part1 = do
	text <- readFile "data/Day20Data.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> do
			return $ product . map (toInteger . fst) . filter ((==2) . length . snd) . friendCounts . map toETile $ parsed

-- Given an orientation, a tile can be represented by a series of 4 numbers (up, right, down, left)
-- For each edge the number is obtained by treating # as 1 and . as 0 and reading the edge clockwise
-- so e.g. a tile [##, .#] has the 3 borders [##, ##, #., .#] and would become [3, 3, 2, 1]
-- tiles can be rotated, which mean this array gets 3 other variants [3213] [2133] [1332]
-- Furthermore tiles can be flipped which changes their edges, so that (with an horizontal flip)
-- The tile becomes [##, #.] which is [##, #., .#, .#] which is [3, 2, 1, 1] 
-- Which also has 3 other rotations [2113] [1132] [1321]
-- For a total of 8 possible position of the borders.

-- A tile represented by its int-edges (regular and flipped)
type ETile = (Name, [Edge], [Edge])
type Name = Int
type Edge = Int

--flipT :: ETile -> ETile
--flipT (name, e) = (name, flipPixels

rotations :: ETile -> [ETile]
rotations (name, a, b) = map (\n -> (name, rotateT a n, rotateT b n)) [0,1,2,3]

rotateT :: [a] -> Int -> [a]
rotateT xs n = (drop n xs) ++ (take n xs)

toETile :: PTile -> ETile
toETile (n, pxls) = (n, edges pxls, edges (flipP pxls)) where
	
edges :: [[Pxl]] -> [Edge]
edges p = map readPixelEdge [
 head p,
 last . transpose $ p,
 last $ p,
 head . transpose $ p
 ]
readPixelEdge = foldl (\acc i -> 2*acc + i) 0 

reade = readPixelEdge . map (digitToInt)

-- Testing / Debugging
	
t0 :: (Int, [[Int]])
t0 = (3321, [[1,1], [0,1]])

t1 :: (Int, [[Int]])
t1 = (5743, [[1,0,1], [1,1,1], [0,0,1]])


test = do
	text <- readFile "data/Day20Data.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> do
			return $ friendCounts . map toETile $ parsed

			
t9 = unsafePerformIO $ test

-- returns a map from tile name to their edges that have friends
friendCounts :: [ETile] -> [(Name, [Int])]
friendCounts tiles = [
	(name, edges) 
	| t0@(name, e, f) <- tiles, 
	let edges = friendEdges . friends t0 $ tiles
	]
	
friendEdges :: [(Name, [Edge])] -> [Int]
friendEdges = nub . join . map snd

friends :: ETile -> [ETile] -> [(Name, [Edge])]
friends t0 tiles = [(name, edges) | t@(name, _, _) <- tiles, t /= t0, let edges = matchingEdges t0 t, not . null $ edges]

matchingEdges :: ETile -> ETile -> [Edge]
matchingEdges (n0, ea, eb) (n1, fa, fb) 
	| n0 == n1 = []
	| otherwise = ea `intersect` (fa ++ fb) 

-- Part2
-- After completing part1 it's clear now that the edge-matching is straightforward.
-- So we'll use a different data representation for the next part that makes things easier
-- We'll start with a corner tile and all matches will be going right or down
--

part2 = do
	text <- readFile "data/Day20tData.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> do
			return $ part2' parsed

trace2 !x = trace (show x) x

part2' tiles = let
	eTiles = map toETile $ tiles
	mTiles = map toMTile $ tiles
	!rCornerTile = findCorner mTiles eTiles
	!mTiles' = concatMap variants mTiles -- all mtiles in their 8 variations
	rawImage = assemble' (trace2 rCornerTile) mTiles'
	in rawImage
{-
	image = removeBorders (trace (show rawImage) rawImage)
	withMonsters = findMonsters image
	hashes = countHashes image 
	in hashes - withMonsters
-}

variants :: MTile -> [MTile]
variants m = [flipf $ rotateM_ n m | n <- [0..3], flipf <- [id, flipM]]

flipM :: MTile -> MTile
flipM (name, e, pxls) = let pxl' = flipP pxls in (name, edges pxl', pxl')

flipP = map reverse

findCorner :: [MTile] -> [ETile] -> MTile
findCorner mTiles eTiles = let
	friends = friendCounts $ eTiles
	corners = map fst . filter ((==2) . length . snd) $ friends	
	(name, _, _) = head [eTile | eTile@(name, _, _) <- eTiles, name == (corners!!0)]
	swEdges = case lookup name friends of Just [e1, e2] -> (e1, e2)
	!corner = rotateCorner mTiles name swEdges
	in flipM . rotateM_ 1 $ corner -- Align with example
		
rotateCorner :: [MTile] -> Int -> (Int, Int) -> MTile
rotateCorner tiles name (e1, e2) = let
	tVariants = variants $ head [t | t@(n, _, _) <- tiles, n == name]
	correct = head [var | var@(_, e, _) <- tVariants, 
		(e!!1 == e1 && e!!2 == e2) || (e!!1 == e2 && e!!2 == e1)] 
	in correct	

type TileMap = M.Map (Int,Int) MTile

assemble' :: MTile -> [MTile] -> TileMap
assemble' corner tiles = foldl (findTile tiles) 
	(trace ("Write " ++ (show (0,0)) ++ " -> " ++ (show corner)) $
	M.singleton (0,0) corner) coords

coords :: [(Int, Int)]
coords = [(x,y) | y <- [0..2], x <- [0..2], (x,y) /= (0,0)]

findTile :: [MTile] -> TileMap -> (Int, Int) -> TileMap
findTile !tiles !tilemap (trace2 -> (x, y)) = let
	seen = map (\(n,_,_) -> n) $ M.elems tilemap
	(x', y', r, edgeDir) = if x == 0
		then (0, y-1, 'S', 'N')
		else (x-1, y, 'E', 'W')
	pred@(name, _, _) = tilemap M.! (x', y')
	edge = readEdge r pred
	newTile = findByEdge tilemap seen edge edgeDir tiles
	in trace ("Write " ++ (show (x,y)) ++ " -> " ++ (show newTile)) $ M.insert (x,y) newTile tilemap
	
type MTile = (Name, [Edge], [[Pxl]])

toMTile :: PTile -> MTile
toMTile (name, pxl) = (name, edges pxl, pxl)

-- Find a matching tile, flipping it if needed
-- findByEdge :: [Int] -> Edge -> Char -> [MTile] -> MTile
findByEdge tilemap seen edge dir tiles = case mapMaybe (findByEdge1 seen edge dir) tiles of
	[tile] -> tile
	[] -> error $ "Found no tile! \n Looking for " ++ (show edge) ++ "\n Seen: " ++ (show seen) ++ "\n Tiles : " ++ (show tilemap)
	xs -> error $ "Found multiple tiles for Edge " ++ (show (edge, dir)) ++ ": "++ (show xs)

-- Find by edge flipping if necessary
findByEdge1 :: [Int] -> Edge -> Char -> MTile -> Maybe MTile
findByEdge1 seen edge dir tile@(n, e, _)
	| n `elem` seen = Nothing
	| (readEdge dir tile) == edge = Just tile
	| otherwise = Nothing

-- Return the south/east edge of a tile
readEdge :: Char -> MTile -> Edge
readEdge 'N' (_, e, _) = e!!0
readEdge 'E' (_, e, _) = e!!1
readEdge 'S' (_, e, _) = e!!2
readEdge 'W' (_, e, _) = e!!3

-- Rotate a tile 90° CW n times
rotateM_ :: Int -> MTile -> MTile
rotateM_ n (name, ee, pxl) = let pxl' = rotateP pxl n in (name, edges pxl', pxl')


-- Apply a 90° rotation n times
rotateP :: [[Pxl]] -> Int -> [[Pxl]]
rotateP !p 0 = p
rotateP !p n = let p' = map reverse $ transpose p in rotateP p' (n-1)

readTilePxl :: MTile -> Int -> Int -> Pxl
readTilePxl (_, _, pxls) x y = pxls!!y!!x

	
type PixelMap = M.Map (Int, Int) Pxl
type Pxl = Int

-- The image is 12x8 = 96 by 96px
pxlCoords = [(tx, ty, px, py) | tx <- [0..11], ty <- [0..11], px <- [1..8], py <- [1..8]]

removeBorders :: TileMap -> PixelMap
removeBorders tiles = foldl (readPxl tiles) M.empty pxlCoords

readPxl :: TileMap -> PixelMap -> (Int, Int, Int, Int) -> PixelMap
readPxl tiles map (tx, ty, px, py) = let
	tile = tiles M.! (tx, ty)
	pix = readTilePxl tile px py
	in M.insert (tx*8 + px, ty*8 + py) pix map


-- 20x3
seaMonster = [
	"                  # ",
	"#    ##    ##    ###",
	" #  #  #  #  #  #   "
	]

monsterCoords0 = [(x,y) | (y, row) <- zip [0..] seaMonster, (x, c) <- zip [0..] row, c == '#']
	
-- Given a top-left coordinate return all pixel coordinates for the monster pattern
-- Or Nothing if any pixel would be out of bounds
monsterCoords :: (Int, Int) -> [(Int, Int)]
monsterCoords (x0, y0) = map (\(x,y) -> (x+x0, y+y0)) monsterCoords0

findMonsters :: PixelMap -> Int
findMonsters pixels = let 
	xys = [(x,y) | x <- [0..76], y <- [0..93]]
	used = length . nub . join . mapMaybe (isMonster pixels) $ xys
	in used

isMonster :: PixelMap -> (Int, Int) -> Maybe [(Int, Int)]
isMonster pixels c0 = 
		if all ((== 1) . (pixels M.!)) $ mC 
		then Just mC 
		else Nothing
	where mC = monsterCoords c0

countHashes :: PixelMap -> Int
countHashes = M.foldr (+) 0 

bToInt :: Int -> Int
bToInt = foldl (\acc i -> 2*acc+i) 0 . map digitToInt . show

