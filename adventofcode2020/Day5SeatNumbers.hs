{-# LANGUAGE ViewPatterns #-} 

import Debug.Trace
import Data.List(sort, (\\))
import Data.Char

dat :: IO String
dat = readFile "data/Day5Data.txt"

-- Part 1

part1 = do
	d <- dat
	return $ maximum [seatid . coords $ s | s <- lines d]
	
coords s | bin <- reverse . map digitToInt $ s = (b (take 3 bin), b (drop 3 bin))
b = foldr (\d a -> 2*a + d) 0

seatid (c, r) = r*8 + c


part2 = do
	d <- dat
	let taken = [seatid . coords $ s | s <- lines d]
	let gaps = [0..1024] \\ taken
	return [x | x <-[1..1023], x`elem`gaps, (x-1)`elem`taken, (x+1)`elem`taken]
	
-- seats 0 to 1024
