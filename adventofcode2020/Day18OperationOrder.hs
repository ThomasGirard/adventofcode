{-# LANGUAGE ViewPatterns #-} 
{-# LANGUAGE BangPatterns #-} 

import Control.Monad (join, forM, forM_, liftM)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.List.Ordered
import qualified Data.Set as S
import Data.Bits

import Control.Applicative (pure, (*>), (<$), (<$>), (<*>))
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
data Expr =
	  Add Expr Expr
	| Mul Expr Expr
	| Val Integer
	deriving (Eq, Show)

p_all :: Parser [Expr]
p_all = endBy p_expr newline

p_expr :: Parser Expr
p_expr = (try p_bin) <|> (try p_par) <|> p_val

p_val = (Val . read . pure) <$> digit
	
p_par = do
	char '('
	sub <- p_expr
	char ')'
	return sub

p_bin = chainl1 p_sub (try p_op)
	
p_sub = try p_val <|> p_par
	
p_op = do
	space
	op <- choice [
		Add <$ char '+',
		Mul <$ char '*'
		]
	space
	return op

-- Eval	
eval :: Expr -> Integer
eval (Val i) = i
eval (Add i j) = (eval i) + (eval j)
eval (Mul i j) = (eval i) * (eval j)
	