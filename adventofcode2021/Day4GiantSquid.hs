import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Control.Applicative
import Control.Monad(join)
import Data.List
import Data.List.Split(chunksOf)

-- Boilerplate
dataPath = "data/Day4Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
type Board = [[Int]]

p_all :: Parser ([Int], [Board])
p_all = do
	numbers <- p_ints
	newline
	boards <- manyTill p_board eof
	return (numbers, boards)
	
p_ints = sepBy H.p_int (char ',')
p_board = newline >> count 5 p_line	
p_line = count 5 p_intspace <* newline  
p_intspace = skipMany (char ' ') >> H.p_int
	
-- Part 1
p1 :: ([Int], [Board]) -> Int
p1 (nums, boards) = head [boardScore called b 
	| called <- inits nums -- consider all prefixes of called numbers in increasing length
	, b <- boards -- and all boards
	, hasWin called b -- select the first board that wins
	]

-- Consider all rows (b) and columns (transpose b). 
-- A row or column is a win iff its difference with called numbers is null (empty).
hasWin :: [Int] -> Board -> Bool
hasWin called b = any null . map (\\ called) $ b ++ transpose b

boardScore :: [Int] -> Board -> Int
boardScore called b = sumUncalled * (last called) where
	sumUncalled = sum . (\\ called) . join $ b

-- Part 2
p2 :: ([Int], [Board]) -> Int
p2 (nums, boards) = p2' [] nums boards

-- Recurse over called numbers, eliminating boards as they're won 
-- Base case, only one board is left. Keep playing it until it's also won, and stop there.
p2' called (n:next) [x] = 
	if hasWin called' x 
		then boardScore called' x 
		else p2' called' next [x]
	where
		called' = called ++ [n]
-- Recursive case
p2' called (n:next) boards = p2' called' next noWinBoards where
	called' = called ++ [n]
	noWinBoards = filter (not . hasWin called') boards

	