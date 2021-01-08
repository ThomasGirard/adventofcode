{-# LANGUAGE ViewPatterns #-} 

import Debug.Trace
import Data.List
import Data.Char

textInput :: IO String
textInput = readFile "data/Day7Data.txt"

-- Part 1

parse :: String -> (String, [(Int, String)])
parse = read

parseAll = sort . map parse . lines

type Rule = (String, [(Int, String)])

part1 = do
	d <- textInput
	let dat = parseAll d
	return $ length . canContain dat $ ["shiny gold"]


canContain1 :: [Rule] -> [String] -> [String]
canContain1 rules t = sort . nub . ((++) t) $[container | (container, subs) <- rules, not . null . intersect (map snd subs) $ t]

-- This gives one too many output bags since it includes the input bag itself
canContain rules t = 
	let 
		n1 = canContain1 rules t
		n2 = canContain1 rules n1
	in (if n1 == n2 then n1 else canContain rules n2)
	
part2 = do
	d <- textInput
	let rules = parseAll d
	return . bagsAllTheWayDown rules $ [(1, "shiny gold")]

test = do
	d <- textInput
	let rules = parseAll d
	return . bagsAllTheWayDown rules $ [(1, "ga")]
	
-- Counts the outer bag(s) which is 1 too many for the puzzle
bagsAllTheWayDown :: [Rule] -> [(Int, String)] -> Int
bagsAllTheWayDown rules [] = 0
bagsAllTheWayDown rules ((n, name):bags) = 
	(bagsAllTheWayDown rules bags) -- count siblings
	+ n -- count these bags
	+ n * (case lookup name rules of
		Just [] -> 0 -- No sub bags
		Just subBags -> bagsAllTheWayDown rules subBags -- Count sub bags
		Nothing -> error $ "not found " ++ name ++ "\n")

