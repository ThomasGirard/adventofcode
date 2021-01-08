import Debug.Trace

dat :: IO String
dat = readFile "data/Day3Data.txt"

-- Part 1

part1 = do
	d <- dat
	let l = zip [0..] (lines d)
	return $ length . filter (== '#') . map tob $ l

tob (n, l) = (cycle l) !! (n * sx)

sx = 3
sy = 1
	
	
-- Part 2

slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

part2 = do
	d <- dat
	let l = zip [0..] (lines d)
	return $ [length . filter (== '#') . map (tob' s) $ l | s <- slopes]

tob' (sx, sy) (n, l) | n `mod` sy > 0 = '.'
tob' (sx, sy) (n, l) = (cycle l) !! ((n `div` sy) * sx)

