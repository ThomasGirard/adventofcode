{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered
import qualified Data.Set as S
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

import qualified Text.ParserCombinators.ReadP as PP

import Debug.Trace (trace)

-- ADT & Parsing
-- TODO this uses a modified file format as a hack because I can't write the right parser
-- Each rule is . terminated
-- Spaces around | have been removed.
type Parsed = ([(Int, PRule)], [String])

data PRule =
	  PChar Char
	| PAlt [[Int]]
	deriving (Eq, Show)

p_all :: Parser Parsed
p_all = do
	rules <- endBy p_rule (char '.' >> newline)
	newline
	messages <- endBy p_message newline
	return (rules, messages)

p_rule = do
	no <- read <$> many1 digit
	char ':' 
	char ' '
	rule <- choice [p_rchar, p_ralt]
	return (no, rule)
	
p_rchar = do
	try $ char '"'
	c <- anyChar
	char '"'
	return $ PChar c

p_ralt = do
	subs <- sepBy p_seq (char '|')
	return $ PAlt subs

p_seq = do
	subs <- p_intlist
	return subs
	
p_intlist = sepBy (read <$> many1 digit) space

p_message = many1 lower

-- Convert from PRule to ERules	
data ERule =
	  EChar Char
	| EAlt [[ERule]]
	deriving (Eq, Show)

part1 = do
	text <- readFile "data/Day19Data.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right (pRules, messages) -> do
			let eRules = compileRules pRules
			let parser = makeParser eRules
			return . sum . map (length . PP.readP_to_S parser) $ messages
			-- return $ length . rights . map (parse parser) $ messages

part2 = do
	text <- readFile "data/Day19Data2.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right (pRules, messages) -> do
			let eRules = compileRules pRules
			let parser = makeParser eRules
			return . sum . map (length . PP.readP_to_S parser) $ messages
			-- return $ length . rights . map (parse parser "") $ messages
			-- But the result is incorrect : (206 too low)
			-- It seems recursion doesn't work as intended, see QU example below
	

-- For part2 this still terminates of lazyness(?), hurrah!
compileRules :: [(Int, PRule)] -> ERule
compileRules rules = compile' rules . findRule rules $ 0
	
compile' rules r = case r of
	PChar c -> EChar c
	PAlt xs -> EAlt $ map (map (compile' rules . findRule rules)) xs
	
findRule rules n = case lookup n rules of
	Just r -> r
	Nothing -> error $ "No rule " ++ (show n)

-- Build a parser based on the rules. Initially I used Parsec for this but it didn't work correctly.
-- It worked OK for part1 but broke for part2 (would accept too few inputs).
-- Using ReadP works instead (and the parser is pretty much the same)
-- This SO answer has an element of explanation : https://stackoverflow.com/a/4280222
-- Parsec is greedy : once a rule has been selected, it never backtracks. But because
-- of how mine was implemented (with shallow try) this was not enough.
-- By contrast ReadP considers ALL parsing branches (which is slower, but correct for this task).
makeParser :: ERule -> PP.ReadP String
makeParser rule = do
	parsed <- makeParser1 rule
	PP.eof
	return parsed

makeParser1 :: ERule -> PP.ReadP String
makeParser1 (EChar c) = pure <$> PP.char c
makeParser1 (EAlt rs) = PP.choice $ map (makeSeqParser) rs

makeSeqParser :: [ERule] -> PP.ReadP String
makeSeqParser [] = pure ""
makeSeqParser (r:rs) = (++) <$> (makeParser1 r) <*> (makeSeqParser rs)
	
-- Testing / Debugging

-- unsafeParsed = unsafePerformIO $ main
-- unsafeRules = compileRules $ fst unsafeParsed
-- unsafeParser = makeParser unsafeRules

test = do
	text <- readFile "data/Day19Data2.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right (pRules, messages) -> do
			let eRules = compileRules pRules
			return pRules
			--let parser = makeParser eRules
			--return $ length . rights . map (parse parser "") $ messages


ex1_ = "0: 4 1 5.\n1: 2 3|3 2.\n2: 4 4|5 5.\n3: 4 5|5 4.\n4: \"a\".\n5: \"b\".\n\n"
ex1 = case parse p_all "" ex1_ of Right (r, m) -> r


{- First tried to make a function to accept a string based on rules
-- But then switched to making a dynamic Parsec parser
accept :: String -> ERule -> Bool
accept s (EAlt rs) = any (acceptSeq s) rs
accept s (EChar c) = s == [c]

acceptSeq :: String -> [ERule] -> Bool
acceptSeq s [] = null s -- All input must be consumed
acceptSeq s (r:rs) = 
acceptSeq _ _ = False 
-}

{- Enumerating is very slow as there are many alternatives. 
-- It did terminate but gave a wrong answer (205) ?? -}
enumerate :: ERule -> [String]
enumerate (EChar c) = [[c]]
enumerate (EAlt rs) = join $ map (\altSeq -> enumerateSeq altSeq) rs

enumerateSeq :: [ERule] -> [String]
enumerateSeq [] = [""]
enumerateSeq (r:rs) = let
	prefixes = enumerate r 
	suffixes = enumerateSeq rs
	in [p++s | p <- prefixes, s <- suffixes]

-- This doesn't work
testStr = unlines [
	"0: 1 2.",
	"2: \"u\".",
	"1: 4|3 3.",
	"3: 1.",
	"4: \"q\".",
	""]
testParser = case parse p_all "" testStr of Right (r,_) -> makeParser . compileRules $ r
	