{-# LANGUAGE ViewPatterns #-} 

import Debug.Trace
import Data.List
import Data.Char

textInput :: IO String
textInput = readFile "data/Day10Data.txt"

-- Part 1

parse :: String -> Integer
parse = read

parseAll = map parse . lines

main = do
	d <- textInput
	let dat = parseAll d
	return (part1 dat, part2 dat)

part1 dat = let (a,b,c) = jmp (0,0,0) (sort $ 0:dat) in (a,b,c)

jmp (a,b,c) [x] = (a, b, c+1) 
jmp (a,b,c) (x:y:z) = 
		let t' = 
			case y-x of 
				1 -> (a+1,b,c)
				2 -> (a,b+1,c)
				3 -> (a,b,c+1)
				n -> error $ "jump of " ++ (show n)
		in jmp t' (y:z)


ex1 = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
ex2 = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]

ex3 = [3,4,5]

-- This is the brute force solution, it's too slow for the complete sequence
part2_ dat = let dat' = (reverse . sort $ 0:dat) in count (3 + head dat') dat'

count n (x:xs) | n-x <= 3 = (count x xs) + (count n xs)
count 0 _ = 1
count n _ = 0

-- But, looking at the delta between steps in the sequence, it will look like this a sequence of 1s and 3s
-- 3s are forced choices. So one can rewrite the sequence as 
-- (n_0 1s) (some 3s) (n_1 1s) (some 3s) (n_2 1s) ...
-- The total number of choices will be the product of the choices for each individual group of 1s which should be ok to bruteforce

part2 (sort -> dat) = let 
	deltas = zipWith (-) dat (0:dat)
	groups = map (\g -> (length g, head g)) . group $ deltas -- (e.g [(5, 1), (2, 3), (2, 1) ...]
	in product [count (n+3) [n,(n-1)..0] | (n, 1) <- groups]





-- Analysis

ex2s = sort ex2
ex2d = zipWith (-) ex2s (0:ex2s) 

-- 3,4,5 is 2 options
-- 3,4,5,6 is 3456 346 356 36 4 options
-- 3,4,5,6,7 is 34567, 3567, 3467, 3457, 367, 347, 357 is 7
-- 345678 is
-- 	1 option skipping none
--  4 options skipping 1
--  6 options skipping 2
--  

