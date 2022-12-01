import qualified Helper as H

import Data.Function ((&))
import Data.List
import Data.Char (digitToInt)
import qualified Data.Map.Strict as M

input = 5235

-- This is slow (3.69s) as it recomputers power for each coordinate many (9?) times.
-- But it works.
part1 = H.maximumOn (sum . map (power input) . mk3x3grid) grids where
	grids = [(x,y) | x <- [1..298], y <- [1..298]]

-- Remplement part1 with the code of part2
-- About 50% faster (1.84s)
part1' = H.maximumOn (sum . map power' . mkGrid) grids where
	power' = (M.!) powerMap
	powerMap = M.fromList [((x,y), power input (x,y)) | x <- [1..300], y <- [1..300]]
	grids = [(x,y,s) | s <- [3], x <- [1..301-s], y <- [1..301-s]]
	
power serial (x,y) = 
	(rackID * y) 
	& (+ serial)
	& (* rackID)
	& hundredsDigit
	& (subtract 5) where
		rackID = x + 10

-- Somehow this is faster than mod (x/100) 10
hundredsDigit = digitToInt . (!!2) . reverse . show

mk3x3grid (x,y) = [(x',y') | x' <- [x..x+2], y' <- [y..y+2]]

-- Part2

-- Precompute power once per coordinate
-- This is still at least one order of magnitude too slow as there are
-- n^3 grids to look at (~27M) and each subgrid takes n^2 reads to the map.
part2 = H.maximumOn (sum . map power' . mkGrid) grids where
	grids = [(x,y,s) | s <- [1..300], x <- [1..301-s], y <- [1..301-s]]
		
power' = (M.!) powerMap
powerMap = M.fromList [((x,y), power input (x,y)) | x <- [1..300], y <- [1..300]]

mkGrid (x,y,s) = [(x',y') | x' <- [x..x+s-1], y' <- [y..y+s-1]]
