{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered
import qualified Data.Set as S
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)

import Control.Applicative (pure, (*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Data.Either (lefts, rights)

import Debug.Trace (trace)

textInput :: IO String
textInput = readFile "data/1.txt"

main = do
	text <- textInput
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right exprs -> return exprs
		
input = unsafePerformIO $ main

p_all = endBy p_int newline
p_int = read <$> many1 digit
		
part1 = sum . map fuel $ input
part2 = sum . map (sum . fuelRec) $ input

fuel :: Integer -> Integer
fuel mass = mass `div` 3 - 2

fuelRec (fuel -> mass) | mass <= 0 = []
  | otherwise = mass:(fuelRec mass)