import Prelude hiding (sum, all, any, concat, concatMap, foldr, mapM_, and, elem)
import Data.Foldable

import qualified Helper as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative

import Data.Char
import Data.Maybe
import Data.List ((\\), elemIndices, intersperse, group, sort)

-- Boilerplate
dataPath = "data/Day12Data.txt"
aoc_main = H.readAndParse dataPath p_all
part1 = aoc_main p1
part2 = aoc_main p2

type Edge = (Cave, Cave)
data Cave = 
	  Small String 
	| Large String 
	deriving (Eq, Show, Ord)
type Path = [Cave]

-- Input parsing
p_all :: Parser [Edge]
p_all = endBy p_line newline
p_line = do
	left <- many1 alphaNum
	char '-'
	right <- many1 alphaNum
	return (mkCave left, mkCave right)
	
mkCave s | all isUpper s = Large s
mkCave s | all isLower s = Small s
mkCave s = error $ "Mixed case name " ++ s

p1 = solve False
p2 = solve True

solve isPart2 edges = length . fromJust $ paths [] (Small "start") where
	
	paths :: Path -> Cave -> Maybe [Path]
	paths visited c 
		| isEnd c = Just [c : visited]
		| canVisit visited c = Just . concat . mapMaybe (paths (c : visited)) . neighbors $ c
		| otherwise = Nothing
		
	neighbors :: Cave -> [Cave]
	neighbors c = (map fst . filter ((==c).snd) $ edges) ++
		(map snd . filter((==c).fst) $ edges)
	
	canVisit visited c 
		| isStart c = null visited
		-- Can visit a large again only if a small was visited since 
		| isLarge c = any isSmall . takeWhile (/= c) $ visited 
		-- first visit to a small is allowed
		| isSmall c && (not $ c `elem` visited) = True
		-- first to be visited twice (Part2 only)
		| isSmall c && isPart2 = all ((==1) . length) . group . sort . filter isSmall $ visited 
		| otherwise = False 

isLarge (Large _) = True
isLarge _ = False
		
isSmall (Small _) = True
isSmall _ = False
		
isEnd (Small "end") = True
isEnd _ = False

isStart (Small "start") = True
isStart _ = False
