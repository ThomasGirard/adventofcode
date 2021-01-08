{-# LANGUAGE ViewPatterns #-} 

import Debug.Trace
import Data.List
import Data.Char

import Control.Applicative ((*>), (<$), (<$>), (<*>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html

textInput :: IO String
textInput = readFile "data/Day8Data.txt"

-- ADT
data Inst = 
	  Acc Int 
	| Jmp Int
	| Nop Int
	deriving (Eq, Read, Show)

-- Parser
p_instr :: Parser (Int -> Inst)
p_instr = choice [
	  Jmp <$ string "jmp"
	, Acc <$ string "acc" 
	, Nop <$ string "nop"
	]

p_val :: Parser Int
p_val = read <$> choice [
		(optional $ char '+') *> digits,
		(:) <$> char '-' <*> digits
	]
	where digits = many1 digit

p_line :: Parser Inst
p_line = do
	instr <- p_instr
	skipMany1 space
	val <- p_val
	return $ instr val

p_all :: Parser [Inst]
p_all = endBy p_line ((string "\n") <|> (string "\r\n"))

-- Part 1
part1 = do
	d <- textInput
	case parse p_all "" d of
		Left e -> error $ "Parse error: " ++ (show e)
		Right prog -> do
			let initState = (prog, [0], 0)
			return $ evalUntilLoop initState

type Program = [Inst]
type InstPointer = Int
-- ExecState is a triplet of 
-- A program, a list of instruction pointers (from most recent to least recently executed) and the accumulator value
type ExecState = (Program, [InstPointer], Int)
	
evalStep :: ExecState -> ExecState
evalStep (prog, ii@(instp:_), acc) = case prog!!instp of
	Nop _ -> (prog, (instp + 1):ii, acc)
	Jmp d -> (prog, (instp + d):ii, acc)
	Acc n -> (prog, (instp + 1):ii, acc + n)

evalUntilLoop :: ExecState -> Int
evalUntilLoop st@(_, ii, acc) = let st'@(_, i':_, _) = evalStep st in
	if i' `elem` ii then acc
	else evalUntilLoop st'

	
-- Part 2 
part2 = do
	d <- textInput
	case parse p_all "" d of
		Left e -> error $ "Parse error: " ++ (show e)
		Right prog -> do
			return $ head [ acc | i <- [0..length prog], Just acc <- [mutateAndExecute prog i]]

-- Now would have been a good time to try and use a Vector
mutateAndExecute prog i = 
	let initState = (prog', [0], 0) 
	in evalSafe initState
	where prog' = [if(j == i) then flipInst inst else inst | (j, inst) <- zip [0..] prog]
			
flipInst (Nop n) = (Jmp n)
flipInst (Jmp n) = (Nop n)
flipInst x = x
			
-- Evaluates the given state until it either loops (return Nothing) or terminates (return the final acc value)
evalSafe :: ExecState -> Maybe Int
evalSafe st@(p, ii, _) = let st'@(_, i':_, acc') = evalStep st in
	if i' `elem` ii then Nothing
	else if i' == (length p) then Just acc'
	else evalSafe st'




