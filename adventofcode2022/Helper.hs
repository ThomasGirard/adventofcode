module Helper (
	  readAndParse 
	, boolToInt
	, countBy
	, groupOn
	, p_lines
	, p_str
	, p_int
	, p_sint
	, none
	, gridToMap
	, mapToGrid
	, mapToGridSparse
	, neighbors4
	, neighbors8
	, maximumOn
	, minimumOn
	, bitsToInt
	) where

import Prelude 

import Data.List hiding (any)
import Data.Ord
import Data.Maybe

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

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
p_lines :: Parser [String]
p_lines = endBy (many1 letter) newline

p_str :: Parser String
p_str = many1 letter

p_int :: Parser Int
p_int = read <$> many1 digit

p_sint :: Parser Int
p_sint = do
	sign <- choice [
		negate <$ char '-',
		id <$ optional (char '+')
		]
	sign . read <$> many1 digit

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
		
mapToGridSparse :: a -> M.Map (Int, Int) a -> [[a]]
mapToGridSparse def mm = 
	[[M.findWithDefault def (r,c) mm | c <- [minC..maxC] ] | r <- [minR..maxR]] where
		maxR = maximum . map fst . M.keys $ mm 
		maxC = maximum . map snd . M.keys $ mm 
		minR = minimum . map fst . M.keys $ mm 
		minC = minimum . map snd . M.keys $ mm 		
	
neighbors4 (r,c) = [(r, c-1), (r, c+1), (r-1, c), (r+1, c)] 
neighbors8 (r,c) = [(r+dr, c+dc) | dr <- [-1..1], dc <- [-1..1]] \\ [(r,c)]

-- | A version of 'maximum' where the comparison is done on some extracted value.
--   Raises an error if the list is empty. Only calls the function once per element.
--
-- > maximumOn id [] == undefined
-- > maximumOn length ["test","extra","a"] == "extra"
maximumOn :: (Ord b) => (a -> b) -> [a] -> a
maximumOn f [] = error "Data.List.Extra.maximumOn: empty list"
maximumOn f (x:xs) = g x (f x) xs
    where
        g v mv [] = v
        g v mv (x:xs) | mx > mv = g x mx xs
                      | otherwise = g v mv xs
            where mx = f x


-- | A version of 'minimum' where the comparison is done on some extracted value.
--   Raises an error if the list is empty. Only calls the function once per element.
--
-- > minimumOn id [] == undefined
-- > minimumOn length ["test","extra","a"] == "a"
minimumOn :: (Ord b) => (a -> b) -> [a] -> a
minimumOn f [] = error "Data.List.Extra.minimumOn: empty list"
minimumOn f (x:xs) = g x (f x) xs
    where
        g v mv [] = v
        g v mv (x:xs) | mx < mv = g x mx xs
                      | otherwise = g v mv xs
            where mx = f x
		
bitsToInt :: [Int] -> Int 
bitsToInt = foldl' (\n d -> n * 2 + d) 0