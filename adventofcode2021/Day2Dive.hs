import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Control.Applicative

-- Boilerplate
dataPath = "data/Day2Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
data Command = Forward Int | Up Int | Down Int

p_all :: Parser [Command]
p_all = endBy p_line newline
p_line = do
	cmd <- p_cmd 
	space 
	dist <- H.p_int
	return $ cmd dist

p_cmd = choice [
	Forward <$ string "forward",
	Up <$ string "up",
	Down <$ string "down"
	]

-- Part 1
p1 :: [Command] -> Int
p1 cmds = dist * depth where
	(dist, depth) = foldl step1 (0,0) cmds
	
type Pos = (Int, Int) -- Depth, X-Dist
step1 :: Pos -> Command -> Pos
step1 (d, x) c = case c of
	Up n -> (d, x-n)
	Down n -> (d, x+n)
	Forward n -> (d+n, x)

-- Part 2
type DState = (Int, Pos) -- Add aim
p2 :: [Command] -> Int
p2 cmds = dist * depth where
	(_, (dist, depth)) = foldl step2 (0,(0,0)) cmds

step2 :: DState-> Command -> DState
step2 (a, p@(d, x)) c = case c of
	Up n -> (a-n, p)
	Down n -> (a+n, p)
	Forward n -> (a, (d + a*n, x+n))
	