{-# LANGUAGE ViewPatterns #-} 

import Debug.Trace
import Data.List
import Data.Char

dat :: IO String
dat = readFile "data/Day6Data.txt"

-- Part 1

part1 = do
	d <- dat
	let gg = g' d
	return $ sum [ length . nub . concat $ g | g <- gg ]
	
g' :: String -> [[String]]
g' = map read . lines


part2 = do
	d <- dat
	let gg = g' d
	return $ sum [ length . foldr intersect ['a'..'z'] $ g | g <- gg ]
	
	
p = foldr (\\) ['a'..'z']