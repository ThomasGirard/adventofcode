{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered
import qualified Data.Set as S
import Data.Bits

import Control.Applicative (pure, (*>), (<*), (<$), (<$>), (<*>))
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Data.Either (lefts, rights)

import Debug.Trace (trace)

textInput :: IO String
textInput = readFile "data/Day18Data.txt"

main = do
	text <- textInput
	case parse p_all "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right exprs -> do
			return $ sum . map eval $ exprs	

-- ADT & Parsing
-- Addition gets evaluated before multiplication means 
-- - A Mul cannot be a subexpr of an Add
-- - The parser must try parsing multiplication before addition

data Expr =
	  Add Expr Expr
	| Mul Expr Expr
	| Val Integer
	deriving (Eq, Show)

p_all :: Parser [Expr]
p_all = endBy p_expr newline

p_expr :: Parser Expr
p_expr = ((try p_mul) <|> (try p_add) <|> (try p_par) <|> p_val)

p_val = (Val . read . pure) <$> digit
	
p_par = do
	char '('
	sub <- p_expr
	char ')'
	return sub

p_mul = chainl1 p_subMul (Mul <$ (try $ string " * "))
p_add = chainl1 p_subAdd (Add <$ (try $ string " + "))
	
p_subMul = (try p_add) <|> (try p_val) <|> (try p_par)
p_subAdd = (try p_val) <|> (try p_par)

-- Eval	
eval :: Expr -> Integer
eval (Val i) = i
eval (Add i j) = (eval i) + (eval j)
eval (Mul i j) = (eval i) * (eval j)