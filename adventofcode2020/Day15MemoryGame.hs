{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered
import qualified Data.IntMap.Strict as M
import Data.Bits

import Control.Applicative ((*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Debug.Trace (trace)

type Say = Int
type Round = Int
type Occurences = M.IntMap Round
type MyState = (Round, Say, Occurences)

input :: [Say]
input = [2,0,1,7,4,14,18]

seed' = handleSeed input

main = do
	return (part1 input, part2 input)

-- Doesn't work, even with BangPatterns we run out of memory (only later)
game seed t = step3 t . handleSeed $ seed
part1 seed = step3 2020 . handleSeed $ seed
part2 seed = step3 30000000 . handleSeed $ seed

handleSeed :: [Say] -> MyState
handleSeed seed = handleSeed' seed 0 M.empty

handleSeed' [s] r mem = (r+1, s, write mem s (r+1))
handleSeed' (s:ss) r mem = handleSeed' ss (r+1) (write mem s (r+1))

step3 target !(currRound, justSaid, !memory) | currRound == target = justSaid
step3 target !(currRound, justSaid, !memory) = step3 target (currRound+1, sayNow, memory')
	where
		memory' = write memory justSaid currRound
		sayNow = case memory !? justSaid of
			Just lastSaid -> currRound - lastSaid
			otherwise -> 0

write occs s n = M.insert s n occs

map !? k = if k `M.member` map then Just $ map M.! k else Nothing
	
ex1 :: [Say]
ex1 = [0,3,6]



			
			
			
				
-- Correct for part1 but too slow/memconsuming for part2
-- (stepList (Left input) M.empty 0) !! 2019 = 496
-- take 20 = [2,0,1,7,4,14,18,0,6,0,2,10,0,3,0,2,5,0,3,5]
stepList :: (Either [Say] Say) -> M.IntMap [Int] -> Int -> [Say] 
stepList (Right said) occs rnd = case occs !? said of
	Just (_:l:_) -> let s' = (rnd-l-1) in (s':) $ stepList (Right s') (write' occs s' rnd) (rnd+1)
	otherwise -> (0:) $ stepList (Right 0) (write' occs 0 rnd) (rnd+1)
stepList (Left [s]) occs rnd = (s:) $ stepList (Right s) (write' occs s rnd) (rnd+1)
stepList (Left (s:ss)) occs rnd = (s:) $ stepList (Left ss) (write' occs s rnd) (rnd+1)

write' occs s n = M.insertWith (++) s [n] occs