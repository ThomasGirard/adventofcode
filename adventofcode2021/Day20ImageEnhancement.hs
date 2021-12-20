{-# LANGUAGE ViewPatterns #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Function ((&), on)

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import Data.Char
import Data.List
import Safe
import Debug.Trace
import Data.Maybe

-- Boilerplate
dataPath = "data/Day20Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = do
	algo <- many1 p_pixel
	newline
	newline
	image <- endBy1 (many1 p_pixel) newline
	return (algo, image)

p_pixel = choice [
	True <$ char '#',
	False <$ char '.'
	]
	
type Algo = [Bool]
type Pixel = Bool
type Image = [[Pixel]]

type Coord = (Int, Int)
type PixelSet = S.Set Coord -- Coordinates that are on

-- The image is infinite
-- But the "outer regions" change predictably.
-- algo[0] is 1 and algo[511] is 0 so the outer pixels flicker between black and white.
-- We will keep track of this as "defaultPixel". This is hardcoded here (0 initially, then 1-defaultPixel each step)
-- But could be defined by reading from the algo array. 

p1 = enhance 2
p2 = enhance 50 

enhance n (algo, image) = S.size . snd . (!!n) . iterate step . initSet $ image where
	
	lookupAlgo bits = (algo !!) . H.bitsToInt $ bits
	
	step (defaultPxl, pixels) = (1-defaultPxl, pixels') where
		(rMin, rMax, cMin, cMax) = getBounds pixels
		inBounds (r,c) = (r <= rMax) && (r >= rMin) && (c <= cMax) && (c >= cMin)
		
		pixels' = S.fromList [(r,c) | r <- [rMin-1..rMax+1], c <- [cMin-1..cMax+1], isOn (r,c) ]
		isOn (r,c) = lookupAlgo bits where
			bits = [bit (r+dr) (c+dc) | dr <- [-1..1], dc <- [-1..1]]
			bit r c 
				| S.member (r,c) pixels = 1 -- Lit up
				| inBounds (r,c) = 0 -- Unlit, within bounds
				| otherwise = defaultPxl -- Out of bounds -> default
		
getBounds :: PixelSet -> (Int, Int, Int, Int)
getBounds s = (
	S.findMin . S.map fst $ s,
	S.findMax . S.map fst $ s,
	S.findMin . S.map snd $ s,
	S.findMax . S.map snd $ s
	)

initSet image = (0, S.fromList onPixels) where 
	onPixels = [(r,c) | (row, r) <- zip image [0..], (pxl, c) <- zip row [0..], pxl]

-- Debugging
printStep :: Int -> [(Int, PixelSet)] -> IO ()
printStep n = mapM_ putStrLn . H.mapToGridSparse '.' . M.fromSet (const '#') . snd . (!!n)
	