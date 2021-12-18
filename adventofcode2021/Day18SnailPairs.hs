{-# LANGUAGE ViewPatterns #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Function ((&), on)

import qualified Data.Map.Strict as M

import Data.Char
import Data.List
import Safe
import Debug.Trace
import Data.Maybe

-- Boilerplate
dataPath = "data/Day18Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

data Snail = 
	  Pair Snail Snail 
	| Single Int 
	deriving (Eq, Show)

-- Input parsing
p_all :: Parser [Snail]
p_all = endBy p_pair newline

p_snail = (try p_single) <|> p_pair

p_single = Single <$> H.p_int

p_pair = do
	char '['
	left <- p_snail
	char ','
	right <- p_snail
	char ']'
	return $ Pair left right

add :: Snail -> Snail -> Snail
add p q = reduce $ Pair p q

reduce :: Snail -> Snail
reduce s | Just s' <- explode s = reduce s'
reduce s | Just s' <- split s = reduce s'
reduce s | otherwise = s

explode :: Snail -> Maybe Snail
explode s = let str = toString s in case findDepth4Index str of
	Just i -> Just . fromString $ explodeAt str i
	Nothing -> Nothing
	
-- Find the first pair of digits that is nested 5 deep and return the index of 
-- its opening bracket
-- My brain thinks this is easier done on the string representation rather than the ADT
findDepth4Index :: String -> Maybe Int
findDepth4Index str = scan str 0 0 where
	-- scan snail index depth 
	scan :: String -> Int -> Int -> Maybe Int
	scan [] _ _ = Nothing
	scan ('[':xs) i 4 = Just i 
	scan ('[':xs) i d = scan xs (i+1) (d+1)
	scan (']':xs) i d = scan xs (i+1) (d-1)
	scan (_:xs) i d = scan xs (i+1) (d)
	
-- Warning, terrible spaghetti ahead

-- Explode the pair starting at index i.
-- This is done by scanning the string left and right of the pair to find the first value
-- that will be exploded onto.
explodeAt str i = let
	(left, pright) = splitAt i str -- left is what precedes the pair
	(pair, ']':right) = break (== ']') pright -- right is what follows
	(pairL, pairR) = readPair (pair ++ "]")
	in (explodeLeft left pairL) ++ "0" ++ (explodeRight right pairR)
	
readPair str = let
	(pairL, r) = span isDigit . tail $ str
	pairR = takeWhile isDigit . tail $ r
	in (pairL, pairR)
	
addStrings a b = show $ (read a) + (read b)
	
explodeRight right pairR = let
	(rightMid, r0) = span (not . isDigit) $ right
	in case r0 of 
		[] -> rightMid -- No value to the right
		otherwise -> let (right, rightRest) = span isDigit r0 in
			rightMid ++ (addStrings pairR right) ++ rightRest
	
explodeLeft (reverse -> leftR) pairL = let
	(leftMid, leftDigit) = span (not . isDigit) $ leftR
	in case leftDigit of
		[] -> (reverse leftMid) -- No value to the left
		otherwise -> let (leftVal, leftRest) = span isDigit leftDigit in
			(reverse leftRest) ++ (addStrings (reverse leftVal) pairL) ++ (reverse leftMid)

split :: Snail -> Maybe Snail
split (Pair p q) | Just p' <- split p = Just (Pair p' q)
split (Pair p q) | Just q' <- split q = Just (Pair p q')
split (Pair p q) | otherwise = Nothing
split (Single n) | n >= 10 = Just $ Pair (Single l) (Single r) where
	l = n `div` 2
	r = (n+1) `div` 2
split (Single n) | n < 10 = Nothing

magnitude :: Snail -> Int
magnitude (Pair p q) = 3*(magnitude p) + 2*(magnitude q)
magnitude (Single n) = n

fromString :: String -> Snail
fromString s = case parse p_snail "" s of
		Left e -> error $ "Parse error: " ++ s ++ " -> " ++ (show e)
		Right parsed -> parsed
		
toString :: Snail -> String
toString (Single n) = show n
toString (Pair p q) = concat ["[", toString p, ",", toString q, "]"]

-- Part 1
p1 = magnitude . foldl1 add 
p2 nums = maximum [magnitude $ add a b | a <- nums, b <- nums, a /= b]

