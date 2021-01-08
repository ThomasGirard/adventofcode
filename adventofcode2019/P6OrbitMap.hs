{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

-- Control
import Control.Monad (join, forM, forM_, liftM)
import Control.Applicative (pure, (*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))

-- Containers
import Data.Maybe
import Data.Either (lefts, rights)
import qualified Data.Tree as T
import qualified Data.Set as S
import qualified Data.IntMap as IM

-- Parsec
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char

-- Misc
import Text.Read (readMaybe)
import Data.Function (on)
import Data.List
import Data.List.Ordered
import Data.Bits

-- Testing/Debugging
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)

textInput :: IO String
textInput = readFile "data/6.txt"

readAndParse = do
	text <- textInput
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right exprs -> return exprs

input = unsafePerformIO $ readAndParse
inputTree = initGraf input

p_all = endBy p_line newline

p_line = do
	orbited <- many1 (upper <|> digit)
	char ')'
	orbiting <- many1 (upper <|> digit)
	return (orbited, orbiting)
	
type Obj = String
type Graf = T.Tree Obj

initGraf :: [(String, String)] -> Graf
initGraf g = let
	g' = groupPairs g
	in T.unfoldTree (findChildren g') "COM"

findChildren :: [(String, [String])] -> String -> (String, [String])
findChildren groups parent = case lookup parent groups of
	Nothing -> (parent, [])
	Just g -> (parent, g)

groupPairs :: (Ord a, Ord b) => [(a, b)] -> [(a, [b])]
groupPairs = map mergeGroup . groupBy ((==) `on` fst) . sort

mergeGroup :: [(a,b)] -> (a, [b])
mergeGroup g@((k,_):_) = (k, map snd g)	
	

-- The number of direct orbits is the number of edges in the graph
directs :: Graf -> Int
directs g = case T.subForest g of
	[] -> 0
	children -> length children + (sum . map directs $ children)
	
-- The number of indirects is the number of edges weighted by their depth in the graph
indirects :: Int -> Graf -> Int
indirects n g = case T.subForest g of
	[] -> 0
	children -> n * (length children) + (sum . map (indirects (n+1)) $ children)

-- Solution
-- One could compute the directs and indirects in a single traversal, which would be faster
part1 tree = (directs tree) + (indirects 0 tree) 
part2 tree = findPath tree "YOU" "SAN"

-- Given two nodes in the tree, compute the path to the root for each, 
-- then find the deepest common ancestor A on both path
-- the distance is the distance from each leaf to A on their path
-- In the example given the path to YOU is [COM, B, C, D, E, J, K, YOU]
-- The path to SAN is [COM, B, C, D, I, SAN]
-- The deepest common ancestor is D.
-- YOU to D is 3, SAN to D is 1 so the total length is 4.
findPath :: Graf -> Obj -> Obj -> Int
findPath tree f t = let
	downTof = fromJust $ pathDown tree f
	downTot = fromJust $ pathDown tree t
	ancestor = last (intersect downTof downTot)
	lengthf = length . dropWhile (/= ancestor) $ downTof
	lengtht = length . dropWhile (/= ancestor) $ downTot
	in lengthf + lengtht - 2 -- Subtract 2 because we're ignoring each last step)
	
pathDown :: Graf -> Obj -> Maybe [Obj]
pathDown tree node 
	| (T.rootLabel tree) == node = Just [] -- Leave out the leaf, it's implicit
	-- Examine all the sub trees and concat them, up to one will have a Just
	| otherwise = case mapMaybe (\sub -> pathDown sub node) (T.subForest tree) of
		[subPath] -> Just $ (T.rootLabel tree):subPath
		[] -> Nothing
		_ -> error "Found multiple paths to leaf ?!"

		
-- Testing
test = T.drawTree $ initGraf ex1'

ex1 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN\n"
ex1' = case parse p_all "" ex1 of Right t -> t
ex1t = initGraf ex1'

printTree str = forM_ (lines . T.drawTree . initGraf $ str) putStrLn