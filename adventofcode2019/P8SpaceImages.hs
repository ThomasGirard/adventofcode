{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

-- Control
import Control.Monad (join, forM, forM_, liftM)
import Control.Applicative (pure, (*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))

-- Containers
import Data.Maybe
import Data.Either (lefts, rights)
import qualified Data.Tree as T
import qualified Data.Set as S
import qualified Data.IntMap as IM

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
import Data.List.Ordered
import Data.Bits

-- Testing/Debugging
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)

textInput :: IO String
textInput = readFile "data/8.txt"

w = 25
h = 6

readAndParse = do
	text <- textInput
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right exprs -> return exprs

input :: [Int]
input = unsafePerformIO $ readAndParse

p_all = do
	pxls <- many1 (digitToInt <$> digit)
	newline
	return $ pxls
	
toLayers = map (chunksOf 25) . chunksOf 150

layers = toLayers input

countPixels :: Int -> [[Int]] -> Int
countPixels like = sum . map (length . filter (==like))

part1 = let layerZero = snd . head . sort . map(\l -> (countPixels 0 l, l)) $ layers 
	in (countPixels 1 layerZero) * (countPixels 2 layerZero)

part2 = [[flatten layers x y | x <- [0..24]] | y <- [0..5]]
flatten layers x y = head . filter (<2) . map ((!!x).(!!y)) $ layers