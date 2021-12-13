import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.Char
import Data.List
import Control.Monad
import Data.Maybe

-- Boilerplate
dataPath = "data/10.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1 >>= mapM_ putStrLn
part2 = aoc_main p2

-- Input parsing
p_all = endBy p_line newline
p_line = do
	string "position=<"
	px <- p_sint
	string ","
	py <- p_sint
	string "> velocity=<"
	vx <- p_sint
	string ","
	vy <- p_sint
	string ">"
	return ((px, py), (vx, vy))

-- Signed int with optional preceding spaces
p_sint = do
	skipMany space
	sign <- choice [
		negate <$ char '-',
		id <$ optional (char '+')
		]
	val <- H.p_int
	return $ sign val

-- Part 1
type Coord = (Int, Int)

-- Guesstimated bounds by looking at the data.
-- Assuming the bounds grow monotonously around the minpoints (which I think is true).
-- One could find the minpoint without having to specify bounds manually.
bounds = [9900..10100]
p1 cs = toString . snd . minimumOn fst $ [(bbsize $ moveN n cs, moveN n cs) | n <- bounds]
moveN n = map (move1 n)
move1 n ((px, py), (vx, vy)) = (px+vx*n, py+vy*n)

-- bounding box size
boundingBox cs = ((minX, maxX), (minY, maxY)) where
	minX = minimum $ map fst cs
	maxX = maximum $ map fst cs
	minY = minimum $ map snd cs
	maxY = maximum $ map snd cs

bbsize :: [Coord] -> Int
bbsize cs = (maxX - minX) * (maxY - minY) where
	((minX, maxX), (minY, maxY)) = boundingBox cs

toString cs = [[if (x,y) `elem` cs then '#' else ' ' | x <- [minX..maxX]] | y <- [minY..maxY]] where
	((minX, maxX), (minY, maxY)) = boundingBox cs

minimumOn f = snd . head . sort . map (\x -> (f x, x))

p2 cs = snd . minimumOn fst $ [(bbsize $ moveN n cs, n) | n <- bounds]
