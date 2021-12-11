import Prelude hiding (map, sum, all, any, concatMap, foldr, mapM_, and)
import Data.Foldable

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative

import Data.Char
import Data.List ((\\))
import qualified Data.Map.Strict as M

-- Boilerplate
dataPath = "data/Day11Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all :: Parser [[Int]]
p_all = endBy p_line newline
p_line = many1 (digitToInt <$> digit)

-- Part 1
p1 = sum . fmap (M.size . M.filter isFlashed) . take 100 . steps . toMap

-- Octopus lifecycle. The Flashing state is transient and never part of a finished step.
-- e.g. an Octopus will start a step as (Charging 9), temporarily be Flashing during computation
-- but finish the step as Flashed.
-- Charging 0 only exists in the seed. After the seed any Octopus that is 0 is Flashed instead.
data Octopus = Charging Int | Flashing | Flashed deriving (Eq, Show)

isFlashing Flashing = True
isFlashing _ = False

isFlashed Flashed = True
isFlashed _ = False

endFlashing Flashing = Flashed
endFlashing x = x

onFlash Flashing = Flashing
onFlash Flashed = Flashed
onFlash (Charging 9) = Flashing
onFlash (Charging n) = Charging $ n+1

type Coord = (Int, Int)
type OctoMap = M.Map Coord Octopus

toMap :: [[Int]] -> OctoMap
toMap grid = M.fromList [
	((r,c), Charging v) 
	| (row, r) <- zip grid [0..]
	, (v, c) <- zip row [0..] ]

-- Infinite list of steps
steps s = let s' = step s in s':(steps s')
step = flash . increment 

increment = M.map incr1
incr1 (Charging 9) = Flashing
incr1 (Charging n) = Charging $ n+1
incr1 Flashed = Charging 1
incr1 Flashing = error "No octopus should be flashing now"

flash = until (H.none isFlashing) flash' where
	flash' om = om'' where
		flashKeys = M.keys . M.filter isFlashing $ om
		neighbors = concatMap neighbors8 $ flashKeys
		om' = M.map endFlashing om
		om'' = foldr (M.adjust onFlash) om' neighbors
	
neighbors8 (r,c) = [(r+dr, c+dc) | dr <- [-1..1], dc <- [-1..1]] \\ [(r,c)]

-- Part 2
p2 = succ . H.findIndexJust (all isFlashed) . steps . toMap 