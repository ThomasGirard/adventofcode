{-# LANGUAGE ViewPatterns #-} 

import Control.Applicative ((*>), (<$), (<$>), (<*>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html

import Debug.Trace
import Data.List
import Data.Char

textInput :: IO String
textInput = readFile "data/Day6pData.txt"

-- Parser	
-- TODO : doesn't work, how can one implement this parser?
p_all :: Parser [[String]]
p_all = endBy p_group (count 2 newline)

p_group = sepBy (many1 lower) newline

ex1 = "a\na\na\na"

-- Part 1
main = do
	d <- textInput
	case parse p_all "" d of
		Left e -> error $ "Parse error: " ++ (show e)
		Right gg -> do
			let part1 = sum [ length . nub . concat $ g | g <- gg ]
			let part2 = sum [ length . foldr intersect ['a'..'z'] $ g | g <- gg ]
			return (part1, part2) -- 828, 565
	
p = foldr (\\) ['a'..'z']