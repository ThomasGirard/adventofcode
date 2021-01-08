import Data.List ((\\))

import Control.Applicative ((*>), (<$), (<$>), (<*>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html

textInput :: IO String
textInput = readFile "data/Day5pData.txt"

-- Parser
fromBin = foldr (\d a -> 2*a + d) 0 . reverse
	
p_row = fromBin <$> many1 ((0 <$ char 'F') <|> (1 <$ char 'B'))
p_col = fromBin <$> many1 ((0 <$ char 'L') <|> (1 <$ char 'R'))
	
p_line :: Parser (Int, Int)
p_line = do
	r <- p_row
	c <- p_col
	return (r, c)
	
p_all :: Parser [(Int, Int)]
p_all = endBy p_line ((string "\n") <|> (string "\r\n"))

-- Part 1
main = do
	d <- textInput
	case parse p_all "" d of
		Left e -> error $ "Parse error: " ++ (show e)
		Right coords -> do
			let part1 = maximum . map seatid $ coords
			let part2 = missingSeat coords
			return (part1, part2) -- 828, 565

seatid (r, c) = r*8 + c

missingSeat d = let 
	taken = map seatid d
	gaps = [0..1024] \\ taken
	in [x | x <-[1..1023], x`elem`gaps, (x-1)`elem`taken, (x+1)`elem`taken]
	