{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.Ord (comparing)
import qualified Data.List.Ordered as OL
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)

import Control.Applicative (pure, (*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Data.Either (lefts, rights)

import Debug.Trace (trace)

input = [2,4,7,8,1,9,3,5,6] :: [Int]
input2 = input ++ [10..1000]
ex1 = [3,8,9,1,2,5,4,6,7] :: [Int]

steps :: [Int] -> [[Int]]
steps x = x : (steps $ step x)

step :: [Int] -> [Int]
step (c:x:y:z:r) = let
	remaining = c:r
	dest = findDest (c-1) remaining
	newCirc = reinsert [x,y,z] remaining dest
	in rotateToNewCurr newCirc c

findDest n cups 
	| n < 0 = maximum cups
	| n `elem` cups = n
	| otherwise = findDest (n-1) cups
	
reinsert removed remaining dest = let
	(left, d:right) = span (/= dest) remaining
	in (d:removed) ++ right ++ left
	
rotateToNewCurr cups curr = let
	(left, c:right) = span (/= curr) cups
	in	if right == [] 
		then cups
		else right ++ left ++ [c]
		
collect cups = toString . init $ rotateToNewCurr cups 1

toString :: (Show a) => [a] -> String
toString = foldr (\c s -> (show c) ++ s) ""

part1 = collect . (!!100) . steps $ input

afterOne :: [Int] -> (Int, Int)
afterOne cups = case dropWhile (/= 1) cups of 
	(_:a:b:r) -> (a, b)
	(_:a:r) -> (a, head cups)
	otherwise -> (cups!!0, cups!!1)
	
-- Is there a pattern that we can optimize on rather than fully simulate?
explore = map (\cups -> (head cups, afterOne cups)) $ steps input2

toTSV = do
	let vals = take 10000 $ explore
	let tsv = unlines . map (\(h,(a,b)) -> 
		join . intersperse "\t" $ [show h, show a, show b]) 
		$ explore
	writeFile "test23.txt" tsv

-- It seems like there isn't

-- Implemented part 2 in Java instead because I'm not sure which data structure would work in Haskell
