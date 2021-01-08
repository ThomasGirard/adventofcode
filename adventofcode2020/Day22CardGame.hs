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

-- ADT & Parsing
type DState = ([Int], [Int])

p_all :: Parser DState
p_all = do
	string "Player 1:"
	newline
	deck1 <- endBy (read <$> many1 digit) newline
	newline
	string "Player 2:"
	newline
	deck2 <- endBy (read <$> many1 digit) newline
	return (deck1, deck2)

part1 = do
	text <- readFile "data/Day22Data.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> do
			return . score . playUntilWinner $ parsed
			

score :: DState -> Int
score ([], x) = score' x
score (x, []) = score' x

score' = sum . map (uncurry (*)) . zip [1..] . reverse
			
playUntilWinner :: DState -> DState
playUntilWinner s0@([], _) = s0
playUntilWinner s0@(_, []) = s0
playUntilWinner s0 = playUntilWinner $ play1 s0

play1 (l:ls, r:rs) = 
	if l > r
	then (ls ++ [l, r], rs)
	else (ls, rs ++ [r, l])
	
-- Part 2

part2 = do
	text <- readFile "data/Day22Data.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> do
			return . score . (\(p,(l,r)) -> if p then (l,r) else (r,l)) . playRecUntilWinner $ parsed

-- The boolean indicates who's the winner (True is p1)
playRecUntilWinner :: DState -> (Bool, DState)
playRecUntilWinner s0 = playRecUntilWinner' [] s0

playRecUntilWinner' prev s0 | s0 `elem` prev = (True, s0)
playRecUntilWinner' _ s0@([], r:rs) = (False, s0)
playRecUntilWinner' _ s0@(l:ls, []) = (True,  s0)
playRecUntilWinner' prev s0@(l:ls, r:rs) 
	| (l > length ls) || (r > length rs) 
		= playRecUntilWinner' (s0:prev) . play1 $ s0
	| otherwise = playRecUntilWinner' (s0:prev) $ 
		if playSub (take l ls, take r rs) 
		then (ls ++ [l, r], rs)
		else (ls, rs ++ [r, l])
	
playSub :: DState -> Bool
playSub s@(l, r) 
	| maximum l > maximum r = True -- This is optimization, code is correct without that branch
	-- Rationale : if p1 has the highest card, 
		-- they will eventually win any subgame since they cannot lose that card
	| otherwise = fst $ playRecUntilWinner s
		
		