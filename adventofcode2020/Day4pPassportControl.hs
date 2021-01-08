{-# LANGUAGE ViewPatterns #-} 

import Debug.Trace
import Data.List(sort, (\\))

dat :: IO String
dat = readFile "data/Day4Data.txt"

-- Part 1

part1 = do
	d <- dat
	return . length . filter valid . map read . lines $ d
	
allFields = sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
	
valid :: [(String, String)] -> Bool
valid = (==allFields) . (\\["cid"]) . sort . map fst

part2 = do
	d <- dat
	return . length . filter valid2 . filter valid . map read . lines $ d

valid2 = all validf
validf ("byr", read -> y) = (y >= 1920) && (y <= 2002)
validf ("iyr", read -> y) = (y >= 2010) && (y <= 2020)
validf ("eyr", read -> y) = (y >= 2020) && (y <= 2030)
validf ("hcl", ('#':col)) = (length col == 6) && (all hexchar col)
validf ("ecl", h) = h `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validf ("pid", h) = (length h == 9) && (all digchar h)
validf ("cid", _) = True
validf ("hgt", h) = validH h
validf _ = False


hexchar c = c `elem` "0123456789abcdef"
digchar c = c `elem` "0123456789"

validH :: String -> Bool
validH h = case span digchar h of
	(read -> v, "cm") -> (v >= 150) && (v <= 193)
	(read -> v, "in") -> (v >= 59) && (v <= 76)
	otherwise -> False