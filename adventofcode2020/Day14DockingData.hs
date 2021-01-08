{-# LANGUAGE ViewPatterns #-} 

import Control.Monad (join)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered
import qualified Data.Map as M
import Data.Bits

import Control.Applicative ((*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Debug.Trace (trace)

readText :: IO String
readText = readFile "data/Day14Data.txt"

type Prog = [Inst]
type Addr = Integer
type MBit = Maybe Int
data Inst = Mask [MBit] | MSet Addr Integer
	deriving (Eq, Show)

-- Program left to execute, current mask, memory map
type Mem = M.Map Addr Integer
type ExecState = (Prog, [MBit], Mem)
	
p_all :: Parser Prog
p_all = endBy p_line newline

p_line = (try p_mask) <|> p_mset

p_mask = do
	string "mask = "
	bits <- many1 p_maskbit
	return $ Mask $ bits

p_mset = do
	string "mem["
	addr <- read <$> many1 digit
	string "] = "
	val <- read <$> many1 digit
	return (MSet addr val)

p_maskbit :: Parser MBit
p_maskbit = choice [
	(Just 0) <$ char '0',
	(Just 1) <$ char '1',
	Nothing <$ char 'X'
	]
	
main = do
	text <- readText
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right program -> return (part1 eState, part2 eState) where eState = (program, [], M.empty)

part1 :: ExecState -> Integer
part1 ([], _, mem) = sumMemory mem 
part1 (p:ps, mask, mem) = case p of
	Mask m -> part1 (ps, m, mem)
	MSet adr val -> part1 (ps, mask, write mem adr (maskval mask val))

sumMemory :: Mem -> Integer
sumMemory = M.foldr (+) 0

write :: Mem -> Addr -> Integer -> Mem
write m a v = M.insert a v m

maskval :: [MBit] -> Integer -> Integer
maskval mask val = maskval' (zip [35,34..0] mask) val

maskval' [] val = val
maskval' ((i, mbit):mask) val = maskval' mask val'
	where val' = case mbit of
		Nothing -> val
		Just 1 -> setBit val i
		Just 0 -> clearBit val i

part2 ([], _, mem) = sumMemory mem 
part2 (p:ps, mask, mem) = case p of
	Mask m -> part2 (ps, m, mem)
	MSet adr val -> part2 (ps, mask, foldr (\m' mem -> write mem m' val) mem $ mems mask adr)

mems :: [MBit] -> Addr -> [Addr]
mems mask a = mems' (zip [35,34..0] mask) a

-- TODO I feel like this could be neatly expressed using the list monad
mems' [] a = [a]
mems' ((i, m):mask) a = case m of
	Just 0 -> mems' mask a
	Just 1 -> map (\adr -> setBit adr i) $ mems' mask a
	Nothing -> let right = mems' mask a in (map (\adr -> setBit adr i) right) ++ (map (\adr -> clearBit adr i) right)
