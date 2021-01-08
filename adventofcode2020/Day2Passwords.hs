
dat :: IO String
dat = readFile "data/Day2Data.txt"

-- Part 1

part1 = do
	d <- dat
	return . length . filter valid . map read . lines $ d

valid :: (Int, Int, Char, String) -> Bool
valid (min, max, c, s) | n <- length . filter (== c) $ s = (n >= min) && (n <= max)
	
-- Part 2

part2 = do
	d <- dat
	return . length . filter valid2 . map read . lines $ d

valid2 :: (Int, Int, Char, String) -> Bool
--valid2 (min, max, c, s) | min == max = (s!!(min-1) == c)
valid2 (min, max, c, s) = (s!!(min-1) == c) `xor` (s!!(max-1) == c)
	
xor True False = True
xor False True = True
xor _ _ = False