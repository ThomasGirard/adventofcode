module Helper (
	  readAndParse 
	, boolToInt
	, countBy
	, groupOn
	, p_int
	, none
	, gridToMap
	, mapToGrid
	, neighbors4
	, neighbors8
	) where

import Prelude 

import Data.List hiding (any)
import Data.Ord
import Data.Maybe

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Control.Applicative

import qualified Data.Map.Strict as M

------------------------------
-- AOC Boilerplate
------------------------------
readAndParse path parser callback = do
	text <- readFile path
	case parse parser "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> return $ callback parsed

------------------------------
-- Parsers
------------------------------
p_int :: Parser Int
p_int = read <$> many1 digit

------------------------------
-- Misc utilities
------------------------------

-- Exists in Data.List.Extra
groupOn f = groupBy ((==) `on2` f)
    -- redefine on so we avoid duplicate computation for most values.
    where (.*.) `on2` f = \x -> let fx = f x in \y -> fx .*. f y

-- Exists in newer GHC.Utils.Misc
countBy :: (a -> Bool) -> [a] -> Int
countBy p = length . filter p

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

none f = not . any f

gridToMap :: [[a]] -> M.Map (Int, Int) a
gridToMap grid = M.fromList [
	((r,c), v) 
	| (row, r) <- zip grid [0..]
	, (v, c) <- zip row [0..] ]
	
mapToGrid :: M.Map (Int, Int) a -> [[a]]
mapToGrid mm = 
	[[mm M.! (r,c) | c <- [0..maxC] ] | r <- [0..maxR]] where
		maxR = maximum . map fst . M.keys $ mm 
		maxC = maximum . map snd . M.keys $ mm 
	
neighbors4 (r,c) = [(r, c-1), (r, c+1), (r-1, c), (r+1, c)] 
neighbors8 (r,c) = [(r+dr, c+dc) | dr <- [-1..1], dc <- [-1..1]] \\ [(r,c)]

		