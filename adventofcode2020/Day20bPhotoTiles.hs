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
import Data.Function (on)
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
type Pxl = [[Int]]
type PTile = (Name, Pxl)
type Name = Int
type Edge = [Int]


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

trace2 !x = trace (show x) x

main = do
	text <- readFile "data/Day20Data.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> return $ (part1 parsed, part2 parsed)

part1 ptiles = ()


-- Return the 8 possible top-left corners (considering flips and rotations)
topLeftCorners :: [PTile] -> [PTile]
topLeftCorners ptilesV = sort
	. map fst
	. filter ((==[DirT, DirL]).snd) -- Only top left corners
	$ neighbors ptilesV 
	
-- Given a set of PTile variants, return a map of each PTile variant to the direction in which they have neighbors
-- E.g. [(1234, [DirT, DirB]), -- 1234 has a neighbor below and top
--       (5678, [DirL]), 	   -- 5678 has a neighbor to the right
neighbors :: [PTile] -> [(PTile, [Dir])]
neighbors ptilesV = map neighbors1 ptilesV where
	neighbors1 t0@(n0, p0) = let 
		ns = sort . nub $ [dir 
			| (n', p') <- ptilesV
			, n0 /= n'
			, Just dir <- [neighborsP p0 p']
			]
		in (t0, ns)

data Dir = DirT | DirR | DirB | DirL
	deriving (Eq, Show, Ord)

-- neighborsP p q returns Nothing if p and q have no common edge, 
-- Just dir if they have exactly one edge in common when p is in the given direction.
-- Ex : if p and q can be assembled with p on top of q, then the return value is Just DirT (p is on top of q).
neighborsP :: Pxl -> Pxl -> Maybe Dir
neighborsP p q = case mapMaybe (\(a,b,dir) -> if a == b then Just dir else Nothing) [pTop, pLeft, pBot, pRight] of
		[] -> Nothing
		[x] -> Just x
		xs -> error "multiple matching edges between 2 tiles" -- Not possible in the given input
	where
	pTop = (last p, head q, DirT)
	pBot = (head p, last q, DirB)
	pLeft = (map last p, map head q, DirL)
	pRight = (map head p, map last q, DirR)

readEdge :: Dir -> Pxl -> [Int]
readEdge DirT = head
readEdge DirB = last
readEdge DirR = map last
readEdge DirL = map head

variants :: PTile -> [PTile]
variants (name, pxl) = [(name, flipf . rotateP n $ pxl) | n <- [0..3], flipf <- [id, hflipP]]

rotateP :: Int -> Pxl -> Pxl
rotateP 0 = id
rotateP 1 = hflipP . transpose
rotateP 2 = reverse . map reverse
rotateP 3 = transpose . hflipP

hflipP :: Pxl -> Pxl
hflipP = map reverse


-- Define which corner to use for part2b (and transform it to align to desired axis)
piecesW = 11
piecesH = 11

getCorners ptiles = ptiles -- neighbors ptiles

part2 ptiles = let
	ptilesV = concatMap variants ptiles
	corners = topLeftCorners ptilesV
	images = map (makeImage ptilesV) corners
	(withMonsters:rest) = reverse . sort . map findMonsters $ images
	in	if (sum rest) /= 0 
		then error "There were monsters in more than one orientation"
		else (countHashes $ images!!0) - withMonsters
		
makeImage :: [PTile] -> PTile -> PixelMap
makeImage ptilesV corner = removeBorders . assemble ptilesV $ (M.singleton (0,0) corner)
	
type TileMap = M.Map (Int, Int) PTile
tileCoords = [(x,y) | y <- [0..piecesH], x <- [0..piecesW], (x,y) /= (0,0)]	
assemble tiles tilemap = foldl (findTile tiles) tilemap tileCoords

findTile :: [PTile] -> TileMap -> (Int, Int) -> TileMap
findTile tiles tmap (x, y) = let
	used = map fst . M.elems $ tmap
	(x', y', dir, dir') = if x == 0
		then (0, y-1, DirB, DirT)
		else (x-1, y, DirR, DirL)
	pred@(name, pxl) = tmap M.! (x', y')
	edge = readEdge dir pxl
	newTile = findByEdge used edge dir' tiles
	in M.insert (x,y) newTile tmap
	
findByEdge :: [Int] -> [Int] -> Dir -> [PTile] -> PTile
findByEdge used edge dir tiles = case [tile 
	| tile@(name, pxl) <- tiles
	, not $ name `elem` used
	, readEdge dir pxl == edge
	]
	of
		[tile] -> tile
		[] -> error "No tile found"
		xs -> error "Multiple tiles found"

		
		
		

readTilePxl :: PTile -> Int -> Int -> Int
readTilePxl (_, pxls) x y = pxls!!y!!x

	
type PixelMap = M.Map (Int, Int) Int

-- The image is 12x8 = 96 by 96px
pxlCoords = [(tx, ty, px, py) | tx <- [0..piecesW], ty <- [0..piecesH], px <- [1..8], py <- [1..8]]

removeBorders :: TileMap -> PixelMap
removeBorders tiles = foldl (readPxl tiles) M.empty pxlCoords

readPxl :: TileMap -> PixelMap -> (Int, Int, Int, Int) -> PixelMap
readPxl tiles map (tx, ty, px, py) = let
	tile = tiles M.! (tx, ty)
	pix = readTilePxl tile px py
	in M.insert (tx*8 + px-1, ty*8 + py-1) pix map


-- 20x3
seaMonster = [
	"                  # ",
	"#    ##    ##    ###",
	" #  #  #  #  #  #   "
	]
monsterXMax = (piecesW+1)*8-20
monsterYMax = (piecesH+1)*8-3
	
monsterCoords0 = [(x,y) | (y, row) <- zip [0..] seaMonster, (x, c) <- zip [0..] row, c == '#']
	
-- Given a top-left coordinate return all pixel coordinates for the monster pattern
-- Or Nothing if any pixel would be out of bounds
monsterCoords :: (Int, Int) -> [(Int, Int)]
monsterCoords (x0, y0) = map (\(x,y) -> (x+x0, y+y0)) monsterCoords0

findMonsters :: PixelMap -> Int
findMonsters pixels = let 
	xys = [(x,y) | x <- [0..monsterXMax], y <- [0..monsterYMax]]
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