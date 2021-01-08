{-# LANGUAGE ViewPatterns #-} 

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Maybe
import Data.List.Split
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered

import Debug.Trace (trace)

dat :: IO String
dat = readFile "data/Day13Data.txt"

-- Part 1
parse_all d = let [s, bb] = lines d in (read s, parse_buses bb)
parse_buses bbs = let bb = splitOn "," bbs in map readMaybe bb
	
main = do
	d <- parse_all <$> dat
	return (part1 d, part2 d)

part1 (t, catMaybes -> bb) = let (tt, b) = minimum [ head [(n*b, b) | n <- [1..], n*b >= t] | b <- bb] in (tt-t)*b

-- part2 :: (Integer, [Maybe Integer]) -> Integer
part2 (snd -> d) = let bb = mapMaybe addOffset . zip [0..] $ d in bb -- findOffset bb

addOffset (o, (Just b)) = Just (o, b)
addOffset _ = Nothing

-- For a given offset+bus this gives all the valid departure times
-- So findOffset must take the deps-lists for all busses and find the smallest common element in all of them
deps :: (Integer, Integer) -> [Integer]
deps (o, b) = [b*n - o | n <- [1..]]

-- Each pair is a (offset, bus#) such that a bus departs at (N * bus# + offset) for all N.
-- findOffset must find the smallest t such that there exists a set of Ns such that for all pairs, t = N_i * Bus_i - offset
findOffset :: [(Integer, Integer)] -> Integer
findOffset bb = let 
	in head . deps . foldl1 combineBusses $ bb
	
-- Combine Busses : Given two busses and their offset, they do repeat over a period and thus can be represented as a new virtual bus
combineBusses bus1@(o1, p1) bus2@(o2, p2) = (p1*p2 - (head $ isect (deps bus1) (deps bus2)), p1*p2)


-- Test data	
ex1 = "0\n17,x,13,19" -- 3417

ex1' = [(0,17), (2, 13), (3,19)]
ex1_test = map (\(o, b) -> (3417 + o) `divMod` b) ex1'

ex2' = [(0,67), (2,7), (3,59), (4, 61)] -- 779210

-- Part2 data (in Haskell form)
pDat = [(0,17),(7,41),(17,643),(25,23),(30,13),(46,29),(48,433),(54,37),(67,19)]
-- Partially reduced by combineBusses, but the last reduction is too slow
pDatCooked = [(2373688529,3886090741), (74091,304399)]

-- Solving for the 2 last busses using wolfram alpha ... 
-- m = 3886090741 k + 2497286064, n = 304399 k + 195614, k element Z (take k=0 then)
s = 2497286064 * 304399 - 74091 -- 760171380521445

-- Check correctness
check x s = map (\(o, p) -> (s+o) `mod` p == 0) x






{-
Given a set of pairs (O_i, B_i) find the smallest t such that there exists a set of N_i and
	for all i, t = B_i * N_i - O_i
	
	This is equivalent to 
		t := O_i mod B_i
		
The Chinese Remainder Theorem??

CRT would apply only for co-prime divisors which all of these are
17 41 643 23 13 29 433 37 19

-------------

https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Statement

The n1..k the divisors (our B_i ?)
The a1..k be the remainders (the O_i)


-}


	
-- Dead ends
-- Trying intersect all on ordered lists (just way too slow)
intersectAll :: [[Integer]] -> [Integer]
intersectAll [x] = x
intersectAll (x:xs) = isect (intersectAll xs) x 
	
-- Brute forcing it (too slow)
iter (t:tt) bb = let bb' = map (dropWhile (<t)) bb in
	if all (==t) (map head bb') then t else (trace (show t) (iter tt bb'))

-- Alternative to list intersection for the last merge, still too slow
-- And afterthought this probably doesn't make sense, n/m should progress as proportions of the existing	
searchCooked [(o1, p1), (o2, p2)] = head [p1*n-o1 | n <- [1..], m <- [1..n], p1*n-o1 == p2*m-o2]

