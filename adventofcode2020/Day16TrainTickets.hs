{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered
import qualified Data.IntMap.Strict as M
import Data.Bits

import Control.Applicative ((*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Debug.Trace (trace)

type Say = Int
type Round = Int
type Occurences = M.IntMap [Round]
type MyState = (Round, Say, Occurences)

p_all = do
	fields <- endBy p_field newline
	newline
	string "your ticket:"
	newline
	mine <- p_ints
	newline
	newline
	string "nearby tickets:"
	newline
	nearby <- endBy p_ints newline
	return (fields, mine, nearby)
	
p_field = do
	location <- many1 $ satisfy (`elem` "abcdefghijklmnopqrstuvwxyz ")
	string ": "
	r1 <- p_range
	string " or "
	r2 <- p_range
	return $ (location, r1, r2)

p_range = do
	f <- p_int
	char '-'
	t <- p_int
	return (f, t)

p_ints = sepBy p_int (string ",")
p_int :: Parser Int
p_int = read <$> many1 digit


------
------

textInput :: IO String
textInput = readFile "data/Day16Data.txt"

main = do
	text <- textInput
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right dat -> do
			return (part1 dat, part2 dat)
			
valueFitsField v (_, r1, r2) = (valueFitsRange v r1) || (valueFitsRange v r2)
valueFitsRange v (f, t) = (f <= v) && (v <= t) 

validTix :: [FieldDef] -> Ticket -> Bool
validTix fields = all (== Nothing) . validateTix fields
validateTix fields tix = map (validateVal fields) tix
validateVal fields v = if any (valueFitsField v) fields then Nothing else Just v
			
part1 (fields, _, tickets) = sum . catMaybes . join $ [validateTix fields tix | tix <- tickets]

part2 (fields, myTix, tickets) = let 
	validTickets = filter (validTix fields) tickets
	fm = buildFieldMap fields validTickets
	in myTicketValue fm myTix

type FieldDef = (String, (Int, Int), (Int, Int))
type FieldMap = [(String, Int)]
type Ticket = [Int]

buildFieldMap :: [FieldDef] -> [Ticket] -> FieldMap
buildFieldMap fields tickets = refineMap $ baseMap fields (transpose tickets)

-- Given all fieldDefs and tickets, build a map of all possible indices for each field.
-- Each field will have 1+ possible column, which are then desambiguated in refineMap
baseMap :: [FieldDef] -> [[Int]] -> [(String, [Int])]
baseMap [] _ = []
baseMap (f@(name, _, _):fi) ttix = (name, indices):(baseMap fi ttix) where indices = fieldIndices f ttix

-- Given a field def, return all the field indices for which this def is valid (i.e. all tickets are in range)
fieldIndices :: FieldDef -> [[Int]] -> [Int]
fieldIndices fdef ttix = [ i | i <- [0..maxFieldNo], all (\v -> valueFitsField v fdef) (ttix!!i) ]

-- Reduce the baseMap such that each field maps to exactly one column
refineMap :: [(String, [Int])] -> FieldMap
refineMap m = reduced where 
	m' = sortOn (length.snd) m -- Sort entries by number of possible columns ascending
	reduced = reduceMap [] m'

reduceMap _ [] = []
reduceMap acc ((name, indices):m) = case indices \\ acc of 
	[x] -> (name, x):(reduceMap (x:acc) m)
	otherwise -> error $ "Coudln't reduce map: " ++ (show otherwise) ++ " at " ++ (show name) 

myTicketValue :: FieldMap -> Ticket -> Integer
myTicketValue fm tix = product . map (toInteger . readField fm tix) $ toRead

readField :: FieldMap -> Ticket -> String -> Int
readField fm tix k = case lookup k fm of 
	Just v -> tix!!v
	Nothing -> error $ "Unknown field "++k

maxFieldNo = 19
toRead = [
	  "departure location"
	, "departure station"
	, "departure platform"
	, "departure track"
	, "departure date"
	, "departure time"
	]
	

ex2 = "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9\n"
toRead' = ["class", "row"]