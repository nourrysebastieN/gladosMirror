
module Lisp
(
    lispFileInterpreter,
    lispCLI
)
where

import Lexer

import Control.Applicative
import Control.Monad
import Control.Monad.Plus
import System.IO
import Data.Char
import Data.Maybe

type StringParser a = Parsec String a

data Constant = Number Int | Boolean Bool deriving (Eq)

instance Show Constant where
    show (Number i) = show i
    show (Boolean b) = show b

data Expression = ConstantExpression Constant | VariableExpression String deriving (Eq, Show)

data Definition = Definition {
    name :: String,
    value :: Expression
} deriving (Eq, Show)

data Form = Def Definition | Expr Expression deriving (Eq, Show)

data LispState = LispState {
    definitions :: [Definition]
}

lexeme :: StringParser a -> StringParser a
lexeme p = whitespace *> p <* whitespace

wrap :: StringParser a -> StringParser a
wrap p = like '(' *> lexeme p <* like ')'



letter :: StringParser Char
letter = label "letter" $ satisfy isAlpha

digit :: StringParser Char
digit = label "digit" $ satisfy isDigit

special :: StringParser Char
special = label "special" $ oneOf "!$%&*/:<=>?~_^"

initial :: StringParser Char
initial = letter <|> digit <|> special <|> oneOf "+-."



constant :: StringParser Constant
constant = label "literal" $ (Number <$> integer) <|> (Boolean <$> ((string "#t" *> pure True) <|> (string "#f" *> pure False)))

identifier :: StringParser String
identifier = label "identifier" $ ((:) <$> initial <*> some initial) <|> (\c -> [c]) <$> (letter <|> special <|> oneOf "+-")

expression :: StringParser Expression
expression = cexpr <|> vexpr
    where
        cexpr = ConstantExpression <$> constant
        vexpr = VariableExpression <$> identifier

_definition :: StringParser Definition
_definition = Definition <$> (lexeme (string "define") *> identifier) <*> lexeme expression

definition :: StringParser Definition
definition = wrap _definition

form :: StringParser Form
form = fdef <|> fexpr
    where
        fdef = Def <$> definition
        fexpr = Expr <$> expression

lispParse :: StringParser [Form]
lispParse = many (lexeme form)

addDefinition :: Definition -> [Definition] -> IO [Definition]
addDefinition def [] = pure [def]
addDefinition def (x:xs) = ((def:xs) <$ guard (name def == name x)) <|> ((x:) <$> addDefinition def xs)

searchDefinition :: String -> [Definition] -> IO (Maybe Definition)
searchDefinition _ [] = pure $ Nothing
searchDefinition n (x:xs) = (pure x) <$ guard (name x == n) <|> searchDefinition n xs

executeExpression :: Expression -> LispState -> IO LispState
executeExpression (ConstantExpression c) s = print c *> pure s
executeExpression (VariableExpression n) s@(LispState defs) = ((\def -> executeExpression (value def) s) =<< (mfromMaybe =<< searchDefinition n defs)) <|> fail ("variable " <> n <> " is not bound")

execute :: Form -> LispState -> IO LispState
execute (Def def) (LispState lst) = LispState <$> addDefinition def lst
execute (Expr e) state = executeExpression e state

lispInterpreter :: [Form] -> LispState -> IO LispState
lispInterpreter [] s = pure s
lispInterpreter (x:xs) state = execute x state >>= (lispInterpreter xs)

lispFileInterpreter :: IO ()
lispFileInterpreter = (getContents >>= (pure . (runParse lispParse))) >>= _result
    where
        _result (Right forms) = () <$ lispInterpreter forms (LispState [])
        _result (Left es) = (putStrLn . show) es

_lispCLI :: LispState -> IO LispState
_lispCLI state = _result =<< putStr "> " *> (hFlush stdout) *> (getLine >>= (pure . (runParse lispParse)))
    where
        _result (Right forms) = _lispCLI =<< lispInterpreter forms state
        _result (Left es) = state <$ (putStrLn . show) es

lispCLI :: IO ()
lispCLI = (() <$ _lispCLI (LispState [])) <|> (putStrLn "")
