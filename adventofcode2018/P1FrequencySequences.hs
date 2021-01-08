{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Prelude hiding (null)

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
textInput = readFile "data/1.txt"

readAndParse = do
	text <- textInput
	return . map (read . remPlus) . lines $ text

remPlus ('+':r) = r
remPlus r = r
	
input :: [Int]
input = unsafePerformIO $ readAndParse

part1 = sum input

part2 freq = head . fst . head . filter hasDup $ iterate step ([0], cycle freq)
step (s:seen, n:next) = ((s+n):s:seen, next)

hasDup (s:seen, _) = s `elem` seen