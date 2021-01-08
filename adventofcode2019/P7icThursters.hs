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
textInput = readFile "data/7.txt"

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
	
-- Execute until output produced (Right) or termination (Left)
execO :: MState -> Either MState MState
execO s = case step s of
	Nothing -> Left s
	Just s' -> 
		if (getOutput s) == (getOutput s') 
		then execO s' -- If outputs match keep running
		else Right s' -- If output changed, stop

getOutput :: MState -> [Int]
getOutput (_, o, _, _) = o

-- Decode and Execute the next instruction
-- A result of Nothing means the program terminated (i.e. the instruction was Term)
step :: MState -> Maybe MState
step s@(i, o, p, m) = let 
	raw = m IM.! p -- raw instruction with opcode and modes
	(dp, inst) = decodeInst p raw
--	in case trace (show inst) $ inst of
	in case inst of
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

part1 = maximum . map runAmpSequence $ settings
settings = permutations [0,1,2,3,4]

runAmpSequence :: [Int] -> Int
runAmpSequence sets = foldl (\amp set -> runAmp set amp) 0 sets

-- Takes as input one of the settings and the previous amp value 
-- and returns the amplified value that is sent to the next amp
runAmp :: Int -> Int -> Int
runAmp set amp = case exec ([set, amp], [], 0, initMem 0 unsafeInput) of
	(_, amp':_, _, _) -> amp'


settings2 = permutations [5,6,7,8,9]
part2 = maximum . map runAmpSequenceR $ settings2

runAmpSequenceR :: [Int] -> Int
runAmpSequenceR sets = let
	amps = map (\set -> ([set], [], 0, initMem 0 unsafeInput)) sets
	in runAmpR amps 0

runAmpR :: [MState] -> Int -> Int
runAmpR ((i, o, p, m):amps) lastV = 
	-- Execute next amp with the previous value appended to the input
	case execO (i ++ [lastV], o, p, m) of
		-- Amp terminated. If it is the last one, return its output as final value
		-- If it's not the last one, proceed but do not schedule this amp for execution again
		Left (i,o,p,m) -> if null amps then (head o) else runAmpR amps (head o)
		-- Amp produced output, continue with next one and queue this one for further execution
		Right amp'@(i,o,p,m) -> runAmpR (amps++[amp']) (head o)	
	
-- Testing and debug

test x = exec ([], [], 0, initMem 0 x)

ex0 :: [Int]
ex0 = [1101,100,-1,4,0]

ex1 :: [Int]
ex1 = [1,9,10,3,2,3,11,0,99,30,40,50]

