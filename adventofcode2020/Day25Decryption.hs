{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.Ord (comparing)
import qualified Data.List.Ordered as OL
import qualified Data.Set as S
import qualified Data.Map.Strict as M
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
import Data.Int
import Debug.Trace (trace)



transforms :: Int64 -> [Int64]
transforms s = iterate (transform1 s) 1
	where transform1 s !v = (s * v) `rem` divisor
	
transform :: Int64 -> Int64 -> Int64
transform lsize subj = go 0 1 where
	go !n !v | n == lsize = v
	go !n !v = go (n+1) ((v * subj) `rem` divisor)

findLoopSize pubK s = case elemIndex pubK (transforms s) of Just ls -> fromIntegral ls

privateKey !pubA !lsB = transform pubA lsB

-- Test


ex_cardPubK =  5764801 :: Int64
ex_doorPubK = 17807724 :: Int64
ex_doorLoopSize = findLoopSize ex_doorPubK 7 -- 11
ex_cardLoopSize = findLoopSize ex_cardPubK 7 -- 8
ex_doorPrivK = privateKey ex_doorLoopSize ex_cardPubK -- 14897079
ex_cardPrivK = privateKey ex_cardLoopSize ex_doorPubK -- 14897079

-- Part1

divisor = 20201227
p1_cardPubK =  9717666 :: Int64
p1_doorPubK = 20089533 :: Int64
p1_doorLoopSize = findLoopSize p1_doorPubK 7 -- 19814867
p1_cardLoopSize = findLoopSize p1_cardPubK 7 -- 17167199
p1_doorPrivK = privateKey 19814867  9717666  -- 3590058
p1_cardPrivK = privateKey 17167199 20089533  -- 7096113 ??