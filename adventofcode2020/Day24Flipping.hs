{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.Ord (comparing)
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

-- A Grid of tiles, with a flat side to the East
-- E-ward, SW-ward
type Coord = (Int, Int)

data Dir = E | SE | SW | W | NW | NE
	deriving (Eq, Show)

move :: Dir -> Coord -> Coord
move d (e, sw) = case d of
	E  -> (e+1, sw)
	W  -> (e-1, sw)
	SW -> (e,   sw+1)
	NE -> (e,   sw-1)
	SE -> (e+1, sw+1)
	NW -> (e-1, sw-1)

p_all :: Parser [[Dir]]
p_all = endBy (many1 p_dir) newline

p_dir = choice [
	E  <$ (string "e"),
	W  <$ (string "w"),
	SW <$ (try $ string "sw"),
	NE <$ (try $ string "ne"),
	SE <$ (string "se"),
	NW <$ (string "nw")
	]

followPath :: Coord -> [Dir] -> Coord
followPath = foldr move	
	
countFlips :: [Coord] -> Int
countFlips = length . filter odd . map length . group . sort
	
part1 = do
	text <- readFile "data/Day24Data.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> do
			return . countFlips . map (followPath (0,0)) $ (trace (show parsed) parsed)

part2 = do
	text <- readFile "data/Day24Data.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> do 
			let init = initMap . map (followPath (0,0)) $ parsed
			return $ S.size $ (steps init)!!100
		
type BlackTiles = S.Set Coord

initMap :: [Coord] -> BlackTiles 		
initMap coords = let
	blackCoords = map fst . filter (odd . snd) . map (\g -> (head g, length g)) . group . sort $ coords
	in S.fromList blackCoords

steps = iterate step

step :: BlackTiles -> BlackTiles
step black = let
	(bx, by) = growBounds . getBounds $ black
	in S.fromList [(x,y) | x <- bToRange bx, y <- bToRange by, isTurnBlack black (x,y)]

bToRange (minV, maxV) = [minV..maxV]
	
getBounds g = S.foldr' mergeBounds b1 g
	where 
		(x0,y0) = (S.elems g)!!0
		b1 = ((x0,x0), (y0,y0))
	
mergeBounds (x,y) (xb, yb) = (mergeBound x xb, mergeBound y yb)

mergeBound v m@(minV, maxV) 
	| v < minV = (v, maxV)
	| v > maxV = (minV, v)
	| otherwise = m
	
growBounds (xb, yb) = (growBound xb, growBound yb)
growBound (minV, maxV) = (minV - 1, maxV + 1)
neighbours (e, sw) = [
	(e+1, sw),
	(e-1, sw),
	(e,   sw+1),
	(e,   sw-1),
	(e+1, sw+1),
	(e-1, sw-1)
	]
	
isTurnBlack :: BlackTiles -> Coord -> Bool
isTurnBlack black c = let
	isCurrBlack = c `S.member` black
	blackN = length . filter (`S.member` black) . neighbours $ c
	in case (isCurrBlack, blackN) of
		(True, 1) -> True
		(True, 2) -> True
		(False, 2) -> True
		_ -> False
		
test = do
	text <- readFile "data/Day24tData.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> do 
			let init = initMap . map (followPath (0,0)) $ parsed
			return $ map S.size $ take 20 $ (steps init)
		

bool f t p = if p then t else f

