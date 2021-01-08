{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)

import Data.Function (on)
import Control.Applicative (pure, (*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Data.Either (lefts, rights)

import Debug.Trace (trace)

inputMin = 273025
inputMax = 767253

isValid p = (pairedDigits p) && (increasingDigits p)

pairedDigits = any ((==2) . length) . group . show
increasingDigits (show -> digits) = all (uncurry (<=)) $ zip digits (tail digits)

part1 = sum [1 | x <- [inputMin..inputMax], isValid x]