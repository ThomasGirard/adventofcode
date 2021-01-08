
dat :: IO String
dat = readFile "data/Day1Data.txt"

solution :: [Int] -> Int
solution d = head [a*b | a <- d, b<- d, a+b == 2020] 

part1 = do
	d <- dat
	return . solution . map read . lines $ d
		
solution2 d = head [a*b*c | a <- d, b<- d, c<- d, a+b+c == 2020] 
	
part2 = do
	d <- dat
	return . solution2 . map read . lines $ d