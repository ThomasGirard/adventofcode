module Helper (
	  readAndParse 
	, boolToInt
	, countBy
	, groupOn
	, p_int
	, none
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
