{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Safe (atDef)
import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Data.Bool
import Text.Read (readMaybe)
import Data.List
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
textInput = readFile "data/9.txt"

unsafeInput :: [Integer]
unsafeInput = unsafePerformIO $ main

main = do
	text <- textInput
	case parse p_all "" text of
		Left e -> error $ "Parse error : " ++ (show e)
		Right parsed -> return parsed

p_all :: Parser [Integer]
p_all = sepBy p_int (char ',')

p_int :: Parser Integer
p_int = do
	sign <- choice [
			negate <$ try (char '-'),
			pure id
		]
	val <- read <$> many1 digit
	return $ sign val

	
	
-- Data Types

data RAdr = RImm Adr | RInd Adr | RRel Adr
	deriving (Eq, Show)

data WAdr = WInd Adr | WRel Adr
	deriving (Eq, Show)
	
type MState = (Input, Output, Ptr, Adr, Mem)
type Input = [Val]
type Output = [Val]
type Adr = Int
type Ptr = Adr
type Rel = Adr
type Val = Integer

type Mem = IM.IntMap Integer
readM :: Mem -> Rel -> RAdr -> Integer
readM !m _ (RImm a) = m !* a
readM !m r (RRel a) = m !* (r + (fromIntegral $ m !* a))
readM !m _ (RInd a) = m !* (fromIntegral $ m !* a)

writeM :: Mem -> Rel -> WAdr -> Integer -> Mem
writeM mem r !wa !v = 
	let adr = 
		case wa of
			WInd a -> fromIntegral $ mem !* a
			WRel a -> r + (fromIntegral $ mem !* a)
	in IM.insert adr v mem

mem !* k = IM.findWithDefault 0 k mem

initMem :: Int -> [Integer] -> Mem
initMem _ [] = IM.empty
initMem n (x:xs) = IM.insert n x (initMem (n+1) xs) 	 

initState :: [Integer] -> [Integer] -> MState
initState prog input = (input, [], 0, 0, initMem 0 prog)

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

getOutput :: MState -> [Integer]
getOutput (_, o, _, _, _) = o

-- Decode and Execute the next instruction
-- A result of Nothing means the program terminated (i.e. the instruction was Term)
step :: MState -> Maybe MState
step s@(i, o, p, r, m) = let 
	raw = m !* p -- raw instruction with opcode and modes
	(dp, inst) = decodeInst p raw
--	in case trace (show inst) $ inst of
	in case inst of
		Term -> Nothing
		otherwise -> Just $ execInst inst (i, o, p+dp, r, m)

-- Decode the raw instruction read at ptr from memory.
-- Return (d_ptr, inst)
-- d_ptr is the pointer offset (depends on the decoded instruction length)
-- and inst is the instruction
decodeInst :: Ptr -> Integer -> (Int, Inst)
decodeInst p raw = let 

	mRAdr :: Integer -> RAdr -- construct the i-th RAdr using the appropriate mode 
	mRAdr (fromIntegral -> i) = case atDef '0' (reverse . show $ raw) (i+1) of
		'2' -> RRel $ p+i
		'1' -> RImm $ p+i
		'0' -> RInd $ p+i
	
	mWAdr :: Integer -> WAdr -- construct the i-th Adr (write)
	mWAdr (fromIntegral -> i) = case atDef '0' (reverse . show $ raw) (i+1) of
		'2' -> WRel $ p+i
		'1' -> error "Immediate write not supported"
		'0' -> WInd $ p+i
		
	
	in case raw `mod` 100 of
	 1 -> (4, Add   (mRAdr 1) (mRAdr 2) (mWAdr 3))
	 2 -> (4, Mul   (mRAdr 1) (mRAdr 2) (mWAdr 3))
	 3 -> (2, Read  (mWAdr 1))
	 4 -> (2, Write (mRAdr 1))
	 5 -> (3, JmpT  (mRAdr 1) (mRAdr 2)) 
	 6 -> (3, JmpF  (mRAdr 1) (mRAdr 2)) 
	 7 -> (4, CmpLT (mRAdr 1) (mRAdr 2) (mWAdr 3))
	 8 -> (4, CmpEQ (mRAdr 1) (mRAdr 2) (mWAdr 3))
	 9 -> (2, RelUp (mRAdr 1))
	 99 -> (1, Term)
	 op -> error $ "Unrecognized opcode " ++ (show op)
		
data Inst = 
	  Add RAdr RAdr WAdr
	| Mul RAdr RAdr WAdr
	| Term
	| Read WAdr
	| Write RAdr
	| RelUp RAdr
	| JmpT RAdr RAdr -- JmpT a b : if a /= 0 set ptr to val-b
 	| JmpF RAdr RAdr -- JmpF a b : if a == 0 set ptr to val-b
	| CmpLT RAdr RAdr WAdr -- CmpLT a b r : if a < b set r to 1, otherwise to 0
	| CmpEQ RAdr RAdr WAdr
	deriving (Eq, Show)

-- Actual logic from the AST
execInst :: Inst -> MState -> MState
execInst inst s0@(i, o, ptr, rel, mem) = case inst of
	(Add a b w) -> execBiOp (+) a b w
	(Mul a b w) -> execBiOp (*) a b w
	(CmpLT a b w) -> execBiOp ((bToInt .).(<)) a b w
	(CmpEQ a b w) -> execBiOp ((bToInt .).(==)) a b w
	
	(Read w) -> case i of 
		(i0:rest) -> (rest, o, ptr, rel, writeM mem rel w i0)
		[] -> error $ "tried to read from empty input " ++ (show (inst, s0))
	(Write a) -> (i, (read' a):o, ptr, rel, mem)
	
	(RelUp a) -> let dr = fromIntegral $ read' a
		in (i, o, ptr, rel+dr, mem)
	
	(JmpT a p) -> execCond p $ (read' a) /= 0
	(JmpF a p) -> execCond p $ (read' a) == 0
	
	Term -> error "Should not happen"
	where 
		execBiOp biOp a b w = (i, o, ptr, rel, writeM mem rel w ((read' a) `biOp` (read' b)))
		execCond p cond = let ptr' = 
					if cond 
					then fromIntegral $ read' p
					else ptr 
			in (i, o, ptr', rel, mem)
		read' = readM mem rel

		
-- Util
bToInt True = 1
bToInt False = 0


part1 = getOutput . exec $ initState unsafeInput [1] 
part2 = getOutput . exec $ initState unsafeInput [2] 

-- Testing

ex0 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] :: [Integer]
ex1 = [1102,34915192,34915192,7,4,7,99,0] :: [Integer]
ex1' = [1102,34915192,34915192,9,9,9,4,0,99,0] :: [Integer]
ex2 = [104,1125899906842624,99] :: [Integer]


testCases = [
	(ex0, [], reverse ex0),
	(ex1, [], [34915192*34915192]),
	(ex2, [], [1125899906842624]),
	([109, -1, 4, 1, 99], [], [-1]),
	([109, -1, 104, 1, 99],[], [1]),
	([109, -1, 204, 1, 99],[], [109]),
	([109, 1, 9, 2, 204, -6, 99],[], [204]),
	([109, 1, 109, 9, 204, -6, 99],[], [204]),
	([109, 1, 209, -1, 204, -106, 99],[], [204]),
	([109, 1, 3, 3, 204, 2, 99], [1234], [1234]),
	([109, 1, 203, 2, 204, 2, 99], [1234], [1234])
	]

runTests testCases = mapM_ runTest testCases
runTest (prog, input, expected) = let output = getOutput . exec $ initState prog input in
	if output == expected 
	then putStrLn "Passed" 
	else putStrLn $ "Expected " ++ (show expected) ++ " got " ++ (show output)





