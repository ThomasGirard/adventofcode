{-# LANGUAGE ViewPatterns #-} 

import Debug.Trace
import Data.List
import Data.Char

import Control.Applicative ((*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html

textInput :: IO String
textInput = readFile "data/Day7pData.txt"


ex1 = "muted blue bags contain 1 vibrant lavender bag, 4 dotted silver bags, 2 dim indigo bags."

-- Parser

p_line :: Parser Rule
p_line = do
	name <- p_name
	string " bags contain "
	subs <- p_subs
	char '.'
	return (name, subs)

p_subs = 
	    ([] <$ string "no other bags")
	<|> (sepBy p_sub1 (string ", "))

p_sub1 :: Parser (Int, String)
p_sub1 = do
	n <- p_int
	spaces
	name <- p_name
	(string " bag") *> (optional $ char 's')
	return (n, name)
	
p_name :: Parser String
p_name = do -- Is there a way to accept any [a-z ] string here rather than constrain it to two words?
	adj <- many1 lower
	space
	color <- many1 lower
	return $ unwords [adj, color]
	
p_int = read <$> many1 digit
	
p_all :: Parser [Rule]
p_all = endBy p_line ((string "\n") <|> (string "\r\n"))

type Rule = (String, [(Int, String)])

main = do
	d <- textInput
	case parse p_all "" d of
		Left e -> error $ "Parse error: " ++ (show e)
		Right rules -> do
			let part1 = length . canContain rules $ ["shiny gold"]
			let part2 = bagsAllTheWayDown rules $ [(1, "shiny gold")]
			return (part1-1, part2-1)



-- Part 1
canContain1 :: [Rule] -> [String] -> [String]
canContain1 rules t = sort . nub . ((++) t) $[container | (container, subs) <- rules, not . null . intersect (map snd subs) $ t]

-- This gives one too many output bags since it includes the input bag itself
canContain rules t = 
	let 
		n1 = canContain1 rules t
		n2 = canContain1 rules n1
	in (if n1 == n2 then n1 else canContain rules n2)

-- Counts the outer bag(s) which is 1 too many for the puzzle
bagsAllTheWayDown :: [Rule] -> [(Int, String)] -> Int
bagsAllTheWayDown rules [] = 0
bagsAllTheWayDown rules ((n, name):bags) = 
	(bagsAllTheWayDown rules bags) -- count siblings
	+ n -- count these bags
	+ n * (case lookup name rules of
		Just [] -> 0 -- No sub bags
		Just subBags -> bagsAllTheWayDown rules subBags -- Count sub bags
		Nothing -> error $ "not found " ++ name ++ "\n")

