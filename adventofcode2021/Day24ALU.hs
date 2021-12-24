{-# LANGUAGE ViewPatterns #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Function ((&), on)

import qualified Data.Set as S

import Data.Char
import Data.List (sort, nub)
import Safe
import Debug.Trace
import Data.Maybe


-- Part 1 and 2
{- 
I initially tried parsing the code and running that but this is clearly not realistic as trillions of hashes
would need to be run. The leftovers of this are below.

Instead I realized the input code had a very repetitive structure. Each input character w
is handled by the same "function" that depends only on the w,z and 3 hard coded parameters (a,b,c).

I was able to reproduce this function as hash1. hash takes a whole input tape and hashes it.

By further analyzing the behavior of hash1 one can see that there are three possible behaviors : 
- When a is 1 the function multiplies z by a factor of ~26.
- When a is 26, the function either divides z by a factor of ~26 or keeps z at the same magnitude.
- In the last iteration, the only way for the final value of z to be 0 is for the input value to be of magnitude 0.
- Because there are 7 iterations of the function where a=1 the magnitude increases by at least 7.
- So each iteration of the function where a=26 the magnitude must decrease.

This is encoded in the function hash1m : 
- When a is 1 the function runs normally.
- When a is 26 the function returns Nothing unless the magnitude can be decreased, halting any further search.

With this knowledge it is reasonable to recursively try digits, stopping the search early when one step cannot
provide the correct magnitude, until a valid hash is found. To find the largest hash, always prefer high branches
in the search tree, and vice-versa for the lowest hash.
-}

part1 = findHash [9,8..1] -- Prefer high numbers
part2 = findHash [1..9] -- Prefer low numbers

findHash :: [Int] -> Maybe String
findHash pref = step (0, params) where
	-- Final z value must be zero
	step (z, [p]) = listToMaybe [[intToDigit d] | d <- pref, hash1m z p d == Just 0] 
	-- Other z values, recurse
	step (z, p:ps) = listToMaybe [(intToDigit d):s 
		| d <- pref
		, Just z' <- [hash1m z p d]
		, Just s <- [step (z', ps)]
		]

hash1m :: Int -> (Int,Int,Int) -> Int -> Maybe Int
hash1m z (1,_,c) w = Just $ (z * 26) + w + c 		
hash1m z p@(26,b,_) w = let
	in  if w == (z `mod` 26) + b
		then Just $ z `div` 26
		else Nothing
	
-- For reference. Unused in solution.	
hash1 :: Int -> (Int,Int,Int) -> Int -> Int
hash1 w (1,_,c) z = (z * 26) + w + c	
hash1 w (26,b,c) z = let
	z' = z `div` 26 
	in  if w == (z `mod` 26) + b
		then z' 
		else (z' * 26) + w + c 		
		
-- These could be parsed from the input
params :: [(Int,Int,Int)]
params = [
	( 1,  11,  8),
	( 1,  12,  8),
	( 1,  10, 12),
	(26,  -8, 10),
	( 1,  15,  2),
	( 1,  15,  8),
	(26, -11,  4),
	( 1,  10,  9),
	(26,  -3, 10),
	( 1,  15,  3), 
	(26,  -3,  7),
	(26,  -1,  7),
	(26, -10,  2),
	(26, -16,  2) 
	]
	
test1 = 99999999999999 -- 210093713
test2 = 12844832362728 -- 2905563944
test3 = 62829194978433 -- 4449320959

---------------------------
-- UNUSED : Parser and VM
---------------------------

-- Boilerplate
dataPath = "data/Day22Data.txt"
aoc_main = H.readAndParse dataPath p_all

parseAndHash :: String -> IO Int
parseAndHash inputModel = aoc_main go where
	go code = let (_,_,_,z) = exec code inputModel in z


--------------
-- VM
--------------	

type Adr = Char
type Registers = (Int, Int, Int, Int)

exec :: [Instr] -> String -> Registers
exec = go (0,0,0,0) where
	go reg [] _ = reg
	go reg (ins:instrs) tape = go reg' instrs tape' where
		(reg', tape') = case ins of
			Inp a -> let (t:tape') = tape in (writeR reg (digitToInt t) a, tape')
			BinOp op a b -> let out = (opFun op) (readR reg a) (readX b) in (writeR reg out a, tape)
		readX (Lit v) = v
		readX (Var a) = readR reg a
		
readR (w,x,y,z) 'w' = w
readR (w,x,y,z) 'x' = x
readR (w,x,y,z) 'y' = y
readR (w,x,y,z) 'z' = z
		
writeR (w,x,y,z) val adr = case adr of 
	'w' -> (val,x,y,z)
	'x' -> (w,val,y,z)
	'y' -> (w,x,val,z)
	'z' -> (w,x,y,val)

		
opFun Add = (+)
opFun Mul = (*)
opFun Mod = mod
opFun Div = div
opFun Eql = (\a b -> H.boolToInt (a == b))

----------------
-- AST & Parser
-----------------	
data Term = Lit Int | Var Char
	deriving (Ord, Eq, Show)

data Instr = 
	  Inp Char
	| BinOp Op Char Term
	deriving (Ord, Eq, Show)
	  
data Op = 
	  Add 
	| Mul 
	| Div 
	| Mod 
	| Eql 
	deriving (Ord, Eq, Show)
	  
p_all :: Parser [Instr]
p_all = endBy p_instr newline

p_instr :: Parser Instr
p_instr = (try p_inp) <|> (try p_binop)

p_inp = (string "inp ") >> (Inp <$> p_wxyz)

p_wxyz = oneOf "wxyz"

p_binop = do
	op <- p_op
	space
	l <- p_wxyz
	space
	r <- p_term
	return $ BinOp op l r

p_term = (Var <$> (try p_wxyz)) <|> (Lit <$> H.p_sint)
	
p_op = choice [
	Add <$ (string "add"),
	Mul <$ (try $ string "mul"),
	Mod <$ (string "mod"),
	Div <$ (string "div"),
	Eql <$ (string "eql")
	]
