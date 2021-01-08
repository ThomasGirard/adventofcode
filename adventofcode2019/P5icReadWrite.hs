{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Safe (atDef)
import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Data.Bool
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)

import Data.Function (on)
import Control.Applicative (pure, (<*), (*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Data.Either (lefts, rights)

import Debug.Trace (trace)

-- Parsing and IO

textInput :: IO String
textInput = readFile "data/5.txt"

unsafeInput :: [Int]
unsafeInput = unsafePerformIO $ main

main = do
	text <- textInput
	case parse p_all "" text of
		Left e -> error $ "Parse error : " ++ (show e)
		Right parsed -> return parsed

p_all :: Parser [Int]
p_all = sepBy p_int (char ',')

p_int :: Parser Int
p_int = do
	sign <- choice [
			negate <$ try (char '-'),
			pure id
		]
	val <- read <$> many1 digit
	return $ sign val

	
	
-- Data Types

data IAdr = Imm Adr | Ind Adr
	deriving (Eq, Show)
	
type MState = (Input, Output, Ptr, Mem)
type Input = [Int]
type Output = [Int]
type Adr = Int
type Ptr = Adr

type Mem = IM.IntMap Int
readM :: Mem -> IAdr -> Int
readM !m (Ind a) = m IM.! (m IM.! a)
readM !m (Imm a) = m IM.! a

writeM :: Mem -> Adr -> Int -> Mem
writeM mem !a !v = IM.insert (readM mem (Imm a)) v mem

initMem :: Int -> [Int] -> Mem
initMem _ [] = IM.empty
initMem n (x:xs) = IM.insert n x (initMem (n+1) xs) 	 

-- Virtual Machine

-- Execute until termination
exec :: MState -> MState
exec s = case step s of 
	Nothing -> s
	Just s' -> exec s'

-- Decode and Execute the next instruction
-- A result of Nothing means the program terminated (i.e. the instruction was Term)
step :: MState -> Maybe MState
step s@(i, o, p, m) = let 
	raw = m IM.! p -- raw instruction with opcode and modes
	(dp, inst) = decodeInst p raw
	in case trace (show inst) $ inst of
		Term -> Nothing
		otherwise -> Just $ execInst inst (i, o, p+dp, m)

-- Decode the raw instruction read at ptr from memory.
-- Return (d_ptr, inst)
-- d_ptr is the pointer offset (depends on the decoded instruction length)
-- and inst is the instruction
decodeInst :: Ptr -> Int -> (Int, Inst)
decodeInst p raw = let 

	rIAdr :: Int -> IAdr -- construct the i-th IAdr using the appropriate mode 
	rIAdr i = case atDef '0' (reverse . show $ raw) (i+1) of
		'1' -> Imm $ p+i
		'0' -> Ind $ p+i
	
	rAdr :: Int -> Adr -- construct the i-th Adr (write)
	rAdr i = (p+i)
	
	in case raw `mod` 100 of
	 1 -> (4, Add   (rIAdr 1) (rIAdr 2) (rAdr 3))
	 2 -> (4, Mul   (rIAdr 1) (rIAdr 2) (rAdr 3))
	 3 -> (2, Read  (rAdr 1))
	 4 -> (2, Write (rIAdr 1))
	 5 -> (3, JmpT  (rIAdr 1) (rIAdr 2)) 
	 6 -> (3, JmpF  (rIAdr 1) (rIAdr 2)) 
	 7 -> (4, CmpLT (rIAdr 1) (rIAdr 2) (rAdr 3))
	 8 -> (4, CmpEQ (rIAdr 1) (rIAdr 2) (rAdr 3))
	 99 -> (1, Term)
	 op -> error $ "Unrecognized opcode " ++ (show op)
		
data Inst = 
	  Add IAdr IAdr Adr
	| Mul IAdr IAdr Adr
	| Term
	| Read Adr
	| Write IAdr
	| JmpT IAdr IAdr -- JmpT a b : if a /= 0 set ptr to val-b
 	| JmpF IAdr IAdr -- JmpF a b : if a == 0 set ptr to val-b
	| CmpLT IAdr IAdr Adr -- CmpLT a b r : if a < b set r to 1, otherwise to 0
	| CmpEQ IAdr IAdr Adr
	deriving (Eq, Show)

-- Actual logic from the AST
execInst inst (i, o, ptr, mem) = case inst of
	(Add a b r) -> execBiOp (+) a b r
	(Mul a b r) -> execBiOp (*) a b r
	(CmpLT a b r) -> execBiOp ((bToInt .).(<)) a b r
	(CmpEQ a b r) -> execBiOp ((bToInt .).(==)) a b r
	
	(Read r) -> let (i0:rest) = i in (rest, o, ptr, writeM mem r i0)
	(Write a) -> (i, (readM mem a):o, ptr, mem)
	
	(JmpT a p) -> execCond p $ (readM mem a) /= 0
	(JmpF a p) -> execCond p $ (readM mem a) == 0
	
	Term -> error "Should not happen"
	where 
		execBiOp biOp a b r = (i, o, ptr, writeM mem r ((readM mem a) `biOp` (readM mem b)))
		execCond p cond = let ptr' = if cond then (readM mem p) else ptr in (i, o, ptr', mem)

		
-- Util
bToInt True = 1
bToInt False = 0

-- Solution

part1 = case exec ([1], [], 0, initMem 0 unsafeInput) of
	(_, output:_, _, _) -> output

part2 = case exec ([5], [], 0, initMem 0 unsafeInput) of
	(_, output:_, _, _) -> output

	

-- Testing and debug

test x = exec ([], [], 0, initMem 0 x)

ex0 :: [Int]
ex0 = [1101,100,-1,4,0]

ex1 :: [Int]
ex1 = [1,9,10,3,2,3,11,0,99,30,40,50]

