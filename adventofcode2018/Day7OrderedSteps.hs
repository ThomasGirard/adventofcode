import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.Char
import Data.List
import Control.Monad
import Data.Maybe

-- Boilerplate
dataPath = "data/7.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing
p_all = endBy p_line newline
p_line = do
	string "Step "
	x <- anyChar
	string " must be finished before step "
	y <- anyChar
	string " can begin."
	return (x,y)

-- Part 1
type Dep = (Char, Char)

p1 :: [Dep] -> String
p1 deps = p1' (allSteps deps) deps

allSteps :: [Dep] -> [Char]
allSteps = sort . nub . join . map (\(x,y) -> [x,y])

p1' rem [] = rem
p1' rem deps = let
	pick = nextStep rem deps
	rem' = rem \\ [pick]
	deps' = cleanUp deps pick
	in pick : (p1' rem' deps') 

cleanUp deps pick = filter ((/= pick) . fst) $ deps

nextStep rem deps = head [r | r <- rem, isFree r deps]
isFree r = all ((/= r) . snd)

-- Part 2
baseCost = 60
lastTask = 'Z'
workers = 5

p2 :: [Dep] -> Int
p2 deps = p2' 0 [] free blocked workers where
	(free, blocked) = partition isFreeTask . allTasks $ deps
	
allTasks :: [Dep] -> [Task]
allTasks deps = sort $ [(c, duration c, parents c) | c <- ['A'..lastTask]] where
	parents t = nub . sort $ [c | (c, x) <- deps, x == t]
	duration c = (ord c) - (ord 'A') + 1 + baseCost

-- A task with its duration and dependencies
type Task = (Char, Int, [Char])
isFreeTask (_, _, []) = True
isFreeTask _ = False
taskDuration (_, d, _) = d

p2' :: Int -- Current time
	-> [Task] -- Tasks in progress
	-> [Task] -- Free tasks
	-> [Task] -- Blocked tasks
	-> Int -- Free workers
	-> Int -- Total duration to complete all tasks
-- When all tasks are done -> finish
p2' t [] [] [] _ = t
-- When no task or worker is available -> finish a task
p2' t (i:inpr) free blocked w | w == 0 || null free = 
	p2' d inpr free' blocked' (w+1) where
		(c, d, []) = i
		(free', blocked') = refreshTasks c (free ++ blocked)
-- Otherwise, start a task
p2' t inpr (new:free) blocked w = 
	p2' t inpr' free' blocked (w-1) where
	new' = case new of (c, d, []) -> (c, d+t, []) :: Task
	free' = free \\ [new]
	inpr' = H.sortOn taskDuration (new':inpr)

-- TODO I think this would be simpler if blocked and free tasks were kept in a single queue
-- The ordering can be maintained by sorting on the parents first (the empty list comes first).

-- Another possible simplification (but this one may make things worst) is to remove the worker parameter
-- and use the length of the inpr queue instead.
	
refreshTasks :: Char -> [Task] -> ([Task], [Task])
refreshTasks t = partition isFreeTask . sort . map (\(c, d, parents) -> (c, d, parents \\ [t]))



