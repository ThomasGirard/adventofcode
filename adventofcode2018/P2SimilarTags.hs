{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Prelude hiding (null)

-- Control
import Control.Monad (join, forM, forM_, liftM, guard)
import Control.Applicative (pure, (*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))

-- Containers
import Data.Maybe
import Data.Either (lefts, rights)
import qualified Data.Tree as T
import qualified Data.Set as S
import qualified Data.IntMap as IM
--import Data.Sequence

-- Parsec
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char

-- Misc
import Text.Read (readMaybe)
import Data.Function (on)
import Data.List
import Data.List.Split(chunksOf)
import qualified Data.List.Ordered as OL
import Data.Bits

-- Testing/Debugging
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)

textInput :: IO String
textInput = readFile "data/2.txt"

readAndParse = do
	text <- textInput
	return . lines $ text
	
input :: [String]
input = unsafePerformIO $ readAndParse

part1 dat = (g 2) * (g 3) where 
	grouped = map (map (\g -> (head g, length g)) . group . sort) $ dat
	g n = length . filter (any ((== n) . snd)) $ grouped
	

ex1 = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]

ex2 = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]

dist a b = length . filter id $ zipWith (/=) a b
diff a b = map fst . filter (uncurry (==)) $ zip a b

part2 dat = head [diff x y | x<-dat, y<-dat, x<y, dist x y == 1]
part2' dat = head $ do
	x <- dat
	y <- dat
	guard (x<y)
	guard (dist x y == 1)
	return $ diff x y
	