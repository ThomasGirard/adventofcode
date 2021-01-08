import Control.Applicative ((*>), (<$), (<$>), (<*>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html

textInput :: IO String
textInput = readFile "data/Day2pData.txt"

-- Parser	
p_line = do
	from <- p_int
	char '-'
	to <- p_int
	char ' '
	key <- letter
	string ": "
	str <- many1 letter
	return (from, to, key, str)

p_int = read <$> many1 digit
	
p_all :: Parser [(Int, Int, Char, String)]
p_all = endBy p_line ((string "\n") <|> (string "\r\n"))

-- Part 1
main = do
	d <- textInput
	case parse p_all "" d of
		Left e -> error $ "Parse error: " ++ (show e)
		Right d -> do
			let part1 = length . filter valid $ d
			let part2 = length . filter valid2 $ d
			return (part1, part2) -- 828, 565


-- Part 1

valid :: (Int, Int, Char, String) -> Bool
valid (min, max, c, s) | n <- length . filter (== c) $ s = (n >= min) && (n <= max)
	
-- Part 2

valid2 :: (Int, Int, Char, String) -> Bool
--valid2 (min, max, c, s) | min == max = (s!!(min-1) == c)
valid2 (min, max, c, s) = (s!!(min-1) == c) `xor` (s!!(max-1) == c)
	
xor True False = True
xor False True = True
xor _ _ = False