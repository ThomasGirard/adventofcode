module Helper (
	  readAndParse 
	, boolToInt
	, countBy
	, p_int
	, maximumOn
	, minimumOn
	, gridToMap
	) where

import Data.List
import Data.Ord
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

gridToMap :: [[a]] -> M.Map (Int, Int) a
gridToMap grid = M.fromList [
	((r,c), v) 
	| (row, r) <- zip grid [0..]
	, (v, c) <- zip row [0..] ]