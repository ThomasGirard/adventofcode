{-# LANGUAGE ViewPatterns #-} 

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Function ((&), on)

import qualified Data.Map.Strict as M

import Data.Char
import Data.List
import Safe
import Debug.Trace

import Control.Monad.Trans.State

-- Boilerplate
dataPath = "data/Day16Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

-- Input parsing (from HexString to a 0-1 String)
type BitString = [Char]

p_all :: Parser BitString
p_all = concat <$> many1 (toBits <$> alphaNum)

toBits '0' = "0000"
toBits '1' = "0001"
toBits '2' = "0010"
toBits '3' = "0011"
toBits '4' = "0100"
toBits '5' = "0101"
toBits '6' = "0110"
toBits '7' = "0111"
toBits '8' = "1000"
toBits '9' = "1001"
toBits 'A' = "1010"
toBits 'B' = "1011"
toBits 'C' = "1100"
toBits 'D' = "1101"
toBits 'E' = "1110"
toBits 'F' = "1111"

bitStrToInt :: BitString -> Integer
bitStrToInt = bitsToInt . map digitToInt where
	bitsToInt = foldl' (\n d -> n * 2 + d) 0 . map fromIntegral

-- ADT for Packets
data Packet =
	  Literal Version Value
	| Operator Version Op [Packet] 
	deriving (Eq, Show)

type Version = Int
type Value = Integer

data Op =
	  Sum
	| Prod
	| Min
	| Max
	| GTo
	| LTo
	| EQo 
	deriving (Eq, Show)

getOp 0 = Sum
getOp 1 = Prod
getOp 2 = Min
getOp 3 = Max
getOp 5 = GTo
getOp 6 = LTo
getOp 7 = EQo
getOp n = error $ "Unknown operator " ++ (show n)
		
-- Define a parser for Packets from BitStrings.
-- This is separate from the input parsing above.
parseOuterPacket :: BitString -> Packet
parseOuterPacket bitString = case parse packetParser "" bitString of
		Left e -> error $ "Packet parse error: " ++ (show e)
		Right parsed -> parsed

packetParser :: Parser Packet
packetParser = do
	version <- p_bitsToInt 3
	typeN <- p_bitsToInt 3
	if typeN == 4
		then p_literal version
		else p_op version typeN
	
-- Read n bits to parse an Int
p_bitsToInt :: Int -> Parser Int
p_bitsToInt n = fromIntegral <$> bitStrToInt <$> count n anyChar

-- Literal parser
-- Iterate over chunks of 5 bits until one starts with 0
p_literal v = p_literal' [] where
	p_literal' acc = do
		(h:bits4) <- count 5 anyChar
		let acc' = acc ++ bits4
		if h == '1' 
			then p_literal' acc'
			else return $ Literal v (bitStrToInt acc') 

-- Operator parser 
p_op v typeN = do
	subs <- p_subs 
	return $ Operator v (getOp typeN) subs

-- Subpacket parser
p_subs = do
	lengthTypeId <- anyChar
	if lengthTypeId == '0' 
	then p_subs_bits
	else p_subs_paks

-- Subpackets by bit count	
p_subs_bits = do
	bitCount <- p_bitsToInt 15
	bits <- count bitCount anyChar
	let p_paks = many (try packetParser)
	-- Start a new parse only on those bits
	let subPackets = case parse p_paks "" bits of
		Left e -> error $ "Bits packet parse error: " ++ (show e)
		Right parsed -> parsed
	return subPackets
	
-- Subpackets by packet count		
p_subs_paks = do
	pakCount <- p_bitsToInt 11
	subPackets <- count pakCount packetParser
	return subPackets

-- Part 1
p1 = sumVersion . parseOuterPacket

sumVersion :: Packet -> Int
sumVersion (Literal v _ ) = v
sumVersion (Operator v _ subs) = (+v) . sum . map sumVersion $ subs

-- Part 2 
p2 = eval . parseOuterPacket
		
eval :: Packet -> Integer
eval (Literal _ v) = v
eval (Operator _ op (map eval -> subs)) = case op of 
	Sum  -> sum     subs
	Prod -> product subs
	Min  -> minimum subs
	Max  -> maximum subs
	GTo  -> boolToInteger (subs!!0 > subs!!1)
	LTo  -> boolToInteger (subs!!0 < subs!!1)
	EQo  -> boolToInteger (subs!!0 == subs!!1)

boolToInteger = fromIntegral . H.boolToInt
