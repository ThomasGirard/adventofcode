import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Control.Applicative
import Control.Monad(join)
import Data.List
import Data.List.Split(chunksOf)

import qualified Data.Map.Strict as M

-- Boilerplate
dataPath = "data/4.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = endBy p_line newline
p_line = do
	time <- p_time
	space
	event <- choice [
		try p_wake,
		try p_sleep,
		try p_begin
		]
	return (time, event)
	
p_time = do
	string "[1518-"
	month <- H.p_int
	char '-'
	day <- H.p_int
	space
	hour <- H.p_int
	char ':'
	minute <- H.p_int
	char ']'
	return (month, day, hour, minute)
	
p_wake = Wake <$ string "wakes up"
p_sleep = Sleep <$ string "falls asleep"
p_begin = do
	string "Guard #"
	no <- H.p_int
	string " begins shift"
	return $ Begin no
	
data Event = Wake | Sleep | Begin Int deriving (Eq, Ord, Show)
type TimeStamp = (Int, Int, Int, Int)

-- Part 1	

-- "Map" of Guard -> [Minutes] where minutes are repeated as they happen
type MinMap = M.Map Int [Int]

p1 events = let 
	(_, _, mmap) = foldl readEvent (-1, (0,0,0,0), M.empty) . sort $ events 
	(maxGuard, mins) = M.foldrWithKey fmax (0, []) $ mmap
	maxMinute = snd . last . sort . map (\g -> (length g, head g)) . group . sort $ mins
	in maxGuard * maxMinute

fmax k' v' (k, v)
	| (length v') > (length v) = (k', v')
	| otherwise = (k, v)
	
readEvent (guard, t, mmap) (t', e) = case e of 
	Begin guard' -> (guard', t', mmap)
	Sleep -> (guard, t', mmap)
	Wake -> (guard, t', mmap') where
		mmap' = onwake mmap guard t t'
	
onwake mmap guard (mo, da, h, m) (mo', da', h', m') = M.alter (fconcat [m..m'-1]) guard mmap

fconcat mins Nothing = Just mins
fconcat mins (Just x) = Just $ x ++ mins 
	
-- Part 2	
-- Same idea but the map is now a map of Minute -> Guards

p2 events = let 
	(_, _, mmap) = foldl readEvent2 (-1, (0,0,0,0), M.empty) . sort $ events
	-- For each minute keep only the best guard as (#Mins, Guard)
	bestPerMinute = M.map (last . sort . map (\g -> (length g, head g)) . group . sort) mmap
	(maxMinute, (_, maxGuard)) = M.foldrWithKey fmax2 (0, (0,0)) $ bestPerMinute
	in maxGuard * maxMinute

fmax2 k' v' (k, v)
	| v' > v = (k', v')
	| otherwise = (k, v)
	
readEvent2 (guard, t, mmap) (t', e) = case e of 
	Begin guard' -> (guard', t', mmap)
	Sleep -> (guard, t', mmap)
	Wake -> (guard, t', mmap') where
		mmap' = onwake2 mmap guard t t'
	
onwake2 mmap guard (mo, da, h, m) (mo', da', h', m') = 
	foldr (\mm mmap -> M.alter (fconcat [guard]) mm mmap) mmap [m..m'-1]
