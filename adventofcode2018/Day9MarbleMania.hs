{-# LANGUAGE BangPatterns #-} 

import qualified Helper as H

import qualified Data.IntMap.Strict as IM
import Data.Maybe (mapMaybe)
import Data.List (groupBy, sort)

type Player = Int
data Marble = 
	  Scored !Int !Player -- Scored by a player
	| Played !Int !Int !Int -- (CCW, N, CW)
	deriving (Eq, Show)

type Marbles = IM.IntMap Marble
type State = (Marble, Marbles, Int) -- (last played, im, player)

play :: Int -> Int -> State -> State

play pcount n (c@(Played m ccw cw), !im, p) | n `mod` 23 == 0 = (m6', im', nextp pcount p) where
	mN' = (Scored n p) -- Score the new marble
	(Played m7 m8 m6) = left . left . left . left . left . left . left $ c
	(Played _ _ m5) = im IM.! m6
	(Played _ m9 _) = im IM.! m8
	m7' = (Scored m7 p) -- Score the marble 7 to the left
	m8' = (Played m8 m9 m6) -- Replace the right neighbor of the marble 8 to the left
	m6' = (Played m6 m8 m5) -- Replace the left neighbor of the marble 6 to the left, this is the new current
	im' = 	insert mN' .
			insert m7' .
			insert m6' .
			insert m8' $ im
	left (Played n l _) = im IM.! l

--(9) - 2 - 5   -    1
-- M    L   N    R   S
-- 9  - 2 - 10 - 5 - 1
-- M isn't changed
-- L is changed to (M, N)
-- R is changed to (N, S)
-- N is inserted as (L, R)
play pcount n ((Played m _ l), !im, p) = (mN', insert mN' . insert mL' . insert mR' $ im, nextp pcount p) where
	(Played _ _ r) = im IM.! l 
	(Played _ _ s) = im IM.! r
	mL' = (Played l m n) 
	mN' = (Played n l r)
	mR' = (Played r n s) 
	
plays max pcount = [initState,initState] ++ (plays' 3 initState) where
	plays' n !s | n > max+1 = []
	plays' n !s = s : (plays' (n+1) $ (play pcount n s))
	
nextp pcount p = (p+1) `mod` pcount
	
insert :: Marble -> Marbles -> Marbles
insert m@(Scored n _) = IM.insert n m
insert m@(Played n _ _) = IM.insert n m

initPlay = 
	insert (Played 2 0 1) .
	insert (Played 1 2 0) .
	insert (Played 0 1 2) $
	IM.empty 
initState = (Played 2 0 1, initPlay, 3)

maxScore :: State -> Int
maxScore (_, im, _) = maximum 
	. map (sum . map snd) 
	. groupOn fst 
	. sort
	. IM.elems
	. IM.map scoreFun
	. IM.filter isScored
	$ im

isScored (Scored _ _) = True
isScored _ = False
	
scoreFun (Scored m p) = (p, m)

groupOn f = groupBy ((==) `on2` f)
    -- redefine on so we avoid duplicate computation for most values.
    where (.*.) `on2` f = \x -> let fx = f x in \y -> fx .*. f y

-- Debugging

inspect :: Marbles -> [Int]
inspect = inspect' 0
inspect' :: Int -> Marbles -> [Int]
inspect' n im = n : (case im IM.! n of (Played _ _ r) -> inspect' r im)

part1 = maxScore . last $ plays 71482 424
part2 = maxScore . last $ plays 7148200 424
