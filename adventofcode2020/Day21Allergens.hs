{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.Ord (comparing)
import qualified Data.List.Ordered as OL
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)

import Control.Applicative (pure, (*>), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Data.Either (lefts, rights)

import Debug.Trace (trace)

-- ADT & Parsing
type Parsed = [PRule]
type PRule = ([Ingredient], [Allergen])
type Ingredient = String
type Allergen = String

p_all :: Parser Parsed
p_all = endBy p_rule newline

p_rule :: Parser PRule
p_rule = (,) <$> (endBy p_ing space) <*> ((try p_allerg) <|> (pure []))

p_allerg :: Parser [String]
p_allerg = do
	string "(contains "
	allerg <- sepBy p_ing (string ", ")
	char ')'
	return allerg
	
p_ing = many1 lower
	
part1 = do
	text <- readFile "data/Day21Data.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> do
			return . countIngredientUses parsed . safeIngredients $ parsed

part2 = do
	text <- readFile "data/Day21Data.txt"
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> do
			let risky = riskyIngredients parsed
			let mapping = createMapping parsed risky
			return . join . intersperse "," $ sortByAllergen mapping risky
			
countIngredientUses :: [PRule] -> [Ingredient] -> Int
countIngredientUses rules ings = let
	ingLists = map fst rules
	in sum . map (\ing -> length . filter (ing `elem`) $ ingLists) $ ings
			
safeIngredients :: [PRule] -> [Ingredient]
safeIngredients rules = let
	allIng = nub . join . map fst $ rules
	risky = riskyIngredients rules
	in allIng \\ risky 
	
riskyIngredients :: [PRule] -> [Ingredient]
riskyIngredients rules = let
	allIng = nub . join . map fst $ rules
	allAll = nub . join . map snd $ rules
	in sort . nub . join $ map (ingredientsForAllergen rules) allAll

ingredientsForAllergen :: [PRule] -> Allergen -> [Ingredient]
ingredientsForAllergen rules all = foldr1 intersect . mapMaybe (
		\(ings, alls) -> if all `elem` alls then Just ings else Nothing
	) $ rules

createMapping :: [PRule] -> [Ingredient] -> [(Ingredient, Allergen)]
createMapping rules risky = let
	allAll = nub . join . map snd $ rules
	allMap = map (\all -> (all, ingredientsForAllergen rules all)) allAll
	in reduceMapping allMap
	
reduceMapping :: [(Allergen, [Ingredient])] -> [(Ingredient, Allergen)]
reduceMapping m = reduce' [] m 

reduce' known [] = known
reduce' known left = let
	single = findSingle known left
	in reduce' ((swap single):known) (remove single left)
	where 
		findSingle known left = head [ (ing, all) | (ing, [all]) <- left]
		remove (ing0, all0) left = [(ing, all \\ [all0]) | (ing, all) <- left, ing /= ing0]
	
sortByAllergen :: [(Ingredient, Allergen)] -> [Ingredient] -> [Ingredient]
sortByAllergen mapping = sortOn (allergenForIngredient mapping)

allergenForIngredient :: [(Ingredient, Allergen)] -> Ingredient -> Allergen
allergenForIngredient mapping ing = fromJust $ lookup ing mapping


swap (a,b) = (b,a)
sortOn f = sortBy (comparing f)




-- Testing and Debugging

ex1 = unlines [ 
	"mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
	"trh fvjkl sbzzf mxmxvkd (contains dairy)",
	"sqjhc fvjkl (contains soy)",
	"sqjhc mxmxvkd sbzzf (contains fish)"
	]
	
test = do
	case parse p_all "" ex1 of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> countIngredientUses parsed . (\x -> trace (show x) x) . safeIngredients $ parsed
