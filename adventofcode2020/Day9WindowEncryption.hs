{-# LANGUAGE ViewPatterns #-} 

import Debug.Trace
import Data.List
import Data.Char

textInput :: IO String
textInput = readFile "data/Day9Data.txt"

-- Part 1

parse :: String -> Integer
parse = read

parseAll = map parse . lines

main = do
	d <- textInput
	let dat = parseAll d
	return (part1 dat, part2 dat)

windowing :: Int -> [a] -> [[a]]
windowing n [] = []
windowing n xs = (take n xs):(windowing n (tail xs)) 
	
part1 dat = head [last window | window <- windowing 26 $ dat, null . checksum . reverse $ window] 

checksum (t:xs) = [(x,y) | x <- xs, y<-xs, x/=y, x+y == t]

part2 dat = sumN 2 dat

part1key = 26134589

sumN n dat = case [minimum seq + maximum seq | seq <- windowing n dat, sum seq == part1key] of
	[] -> sumN (n+1) dat
	x:_ -> x