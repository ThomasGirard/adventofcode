{-# LANGUAGE DeriveFoldable #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import qualified Data.Foldable as F
import Control.Applicative ((<*))
import Safe

-- Boilerplate
dataPath = "data/8.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all :: Parser (Tree Int)
p_all = do
	tree <- p_node
	eof
	return tree

p_node = do
	childCount <- p_intspace
	metaCount <- p_intspace
	children <- count childCount p_node
	meta <- count metaCount p_intspace
	return (Node meta children)
	
data Tree a = Node [a] [Tree a] deriving (Eq, Show, F.Foldable)
empty = Node [] []

p_intspace = H.p_int <* (optional space)

-- Part 1
-- Comes for free with Foldable over metadata..
p1 = F.sum

-- Part 2
p2 = val

val (Node m []) = sum m
val (Node m c) = sum [val . getChild $ i | i <- m] where
	getChild i = atDef empty c (i-1)