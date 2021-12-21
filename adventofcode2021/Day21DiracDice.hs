{-# LANGUAGE ViewPatterns #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Function ((&), on)

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import Data.Char
import Data.List
import Data.Either
import Safe
import Debug.Trace
import Data.Maybe

-- Part 1	
part1 = fromLeft (error "No result") $ play (10,3) deterDice where
	deterDice = cycle [1..100]

play startPos dice = until isLeft playTurn $ (initState startPos dice) where
	initState (p1p, p2p) dice = Right ([(p1p, 0),(p2p, 0)], 0, dice)

	
type PosScore = (Int, Int) -- position and score for a player
type Dice = [Int]
-- Left is the winning score.
-- Right is the current game state.
type GameState = Either Int (
	[PosScore], -- Players, in the order of play
	Int, -- Number of rolls so far
	Dice -- Values of next rolls
	)

playTurn :: GameState -> GameState
playTurn (Right ((pPos, pScore):ps, ri, ds)) = 
	if pScore' < 1000 
	then Right (ps ++ [(pPos', pScore')], ri+3, ds')
	else Left ((ri+3) * p2Score)
	where
	(_, p2Score) = last ps
	(sum -> roll, ds') = splitAt 3 ds -- roll thrice
	pPos' = ((pPos + roll - 1) `mod` 10) + 1
	pScore' = pScore + pPos'	
playTurn (Left _) = error "Cannot play, game is over."


{- For Part 2, given the numbers in the example, there is no way to simulate or enumerate. This has to be a counting problem.
None of the above can be reused.

Observations : 
- There are about 8e14 universes in the example. This means about 31 rolls.
- For each turn, there are 3^3 = 27 possible dice rolls but only 7 values (range 3 to 9 with different probabilities) the dice can take.
- We can simplify the game state since we only care about who won, not how. E.g.
	Player 1 won with 25 points after 18 rolls
	Player 1 won with 24 points after 15 rolls
	Are the same.
	
	This means that in the game tree, a state is only identified by 
		(p1score, p1pos, p2score, p2pos, nextPlayer)
	
	With 10 possible positions and 21 possible non winning scores, there are 
	about 10*10*21*21*2 = 88200 states to keep track of.
	
	It seems feasible to keep track of the count for each, like we've done on
	the fishes and polymer problems.
--}

data St2 = 
-- 			Score1 Pos1 Score2 Pos2
	  P1Plays Int Int Int Int
	| P2Plays Int Int Int Int
	| P1Won
	| P2Won 
	deriving (Ord, Eq, Show)

-- Maps state to the number of universes they occur in.
type StateMap = M.Map St2 Integer

-- Distribution of a 3d3 roll
diracRoll = [
	(3,1),
	(4,3),
	(5,6),
	(6,7),
	(7,6),
	(8,3),
	(9,1)
	]

foreach = flip map
	
-- Given a state, gives the next possible states and their frequency
nextStates :: St2 -> [(St2, Integer)]
nextStates P1Won = [(P1Won, 1)]
nextStates P2Won = [(P2Won, 1)]
nextStates (P1Plays ps pp qs qp) = foreach diracRoll (\(v, c) -> 
		let newPos = ((pp + v - 1) `mod` 10) + 1 in
		if ps + newPos >= 21 
		then (P1Won, c)
		else ((P2Plays (ps + newPos) newPos qs qp), c)
		)
nextStates (P2Plays ps pp qs qp) = foreach diracRoll (\(v, c) -> 
		let newPos = ((qp + v - 1) `mod` 10) + 1 in
		if qs + newPos >= 21 
		then (P2Won, c)
		else ((P1Plays ps pp (qs + newPos) newPos), c)
	)	
	
part2 = 
	initMap (10, 3)
	& until isDone play2
	& M.elems
	& maximum

initMap (p, q) = M.singleton (P1Plays 0 p 0 q) 1

play2 :: StateMap -> StateMap
play2 sm = M.foldrWithKey playState M.empty sm where
	-- For each state in the map, play all possible outcomes
	playState st nTimes newMap = foldr (addNewState nTimes) newMap (nextStates st)
	addNewState nTimes (st', c) = M.alter (incr (nTimes * c)) st'

incr n (Just x) = Just $ n+x
incr n Nothing = Just n
	
-- Only the winning states remain
isDone :: StateMap -> Bool
isDone = (2==) . M.size 
