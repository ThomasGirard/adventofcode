import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.List

-- Boilerplate
dataPath = "data/Day10Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all :: Parser [String]
p_all = endBy p_line newline
p_line = many1 (oneOf "[](){}<>")

-- Part 1
p1 = sum . map (score . head) . filter (not . null) . map illegalChars

score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

isOpen = (flip elem) "[({<"
isClose = (flip elem) "])}>"

matches '(' ')' = True
matches '<' '>' = True
matches '{' '}' = True
matches '[' ']' = True
matches _ _ = False

illegalChars :: String -> [Char]
illegalChars = go [] where
	go _ [] = []
	go os (x:xs) 
		| isOpen x = go (x:os) xs
		| isClose x && null os = go os xs
		| isClose x && (head os) `matches` x = go (tail os) xs
		| otherwise = x:(go os xs) -- Illegal
	

p2 xs = takeMiddle . sort . map (score2 . autoComplete) $ incompletes where
	incompletes = filter (null . illegalChars) xs

autoComplete :: String -> String
autoComplete = map flipParen . go [] where
	go leftover [] = leftover
	go os (x:xs) 
		| isOpen x = go (x:os) xs
		| isClose x && null os = go os xs
		| isClose x && (head os) `matches` x = go (tail os) xs
	
takeMiddle xs | n <- length xs = xs !! (n `div` 2) 

score2 :: String -> Integer
score2 = foldl (\s c -> s*5 + (score2' c)) 0 where
	score2' ')' = 1
	score2' ']' = 2
	score2' '}' = 3
	score2' '>' = 4

flipParen '(' = ')'
flipParen '{' = '}'
flipParen '<' = '>'
flipParen '[' = ']'
