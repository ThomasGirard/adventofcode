module Helper (
	  readAndParse 
	, boolToInt
	, countBy
	, p_int
	, sortOn
	, findIndexJust
	, none
	) where

import Prelude hiding (map, sum, all, any, concatMap, foldr, mapM_, and)
import Data.Foldable

import Data.List hiding (any)
import Data.Ord
import Data.Maybe

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Control.Applicative

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

-- Exists in newer GHC.Utils.Misc
countBy :: (a -> Bool) -> [a] -> Int
countBy p = length . filter p

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

-- https://hackage.haskell.org/package/base-4.16.0.0/docs/src/Data.OldList.html#sortOn
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
  
-- Exists in newer Safe
findIndexJust op = fromJust . findIndex op

none f = not . any f
