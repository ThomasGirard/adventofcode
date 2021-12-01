{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Applicative
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Helper

dataPath = "data/Day1Data.txt"
part1 = readAndParse dataPath p_all p1
part2 = readAndParse dataPath p_all p2

p_all :: Parser [Int]
p_all = endBy p_int newline
p_int = read <$> many1 digit
		
p1 ds = length . filter (uncurry (<)) . zip ds $ (tail ds)
	
p2 ds@(a:b:c:d:_) = isIncr1 + (p2 . tail $ ds) where
	isIncr1 = boolToInt $ (a+b+c) < (b+c+d) -- (This is a < d) but sticking to spec :o)
p2 _ = 0