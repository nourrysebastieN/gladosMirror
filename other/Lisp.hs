{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

module Lisp
(
    lispFileInterpreter,
    lispCLI
)
where

import Monoparsec

import Control.Applicative
import Control.Monad
import Control.Monad.Plus
import Control.Exception (try, SomeException)
import System.IO
import Data.Char
import Data.Maybe()
import Data.List (intercalate)
import Data.Bifunctor()

import Option

type StringParser a = Parsec String CompilerOption a

data Constant = Number Int | Boolean Bool deriving (Eq)

instance Show Constant where
    show (Number i) = show i
    show (Boolean True) = "#t"
    show (Boolean False) = "#f"

data Expression =
        ConstantExpression Constant
    |
        VariableExpression String
    |
        CallExpresssion Expression [Expression]
    |
        Procedure String [String] [Expression]
    |
        Builtin String Int ([Expression] -> LispState -> IO Expression)
    |
        Lambda [String] [Expression]
    |
        If Expression Expression Expression

instance Show Expression where
    show (ConstantExpression c) = show c
    show (VariableExpression n) = n
    show (CallExpresssion _ _) = "call"
    show (Procedure n _ _) = "#<procedure " <> n <> ">"
    show (Builtin n _ _) = "#<procedure " <> n <> ">"
    show (Lambda _ _) = "#<procedure>"
    show (If _ _ _) = "if"

instance Eq Expression where
    (==) (ConstantExpression a) (ConstantExpression b) = a == b
    (==) (Procedure a _ _) (Procedure b _ _) = a == b
    (==) (Builtin a _ _) (Builtin b _ _) = a == b
    (==) _ _ = False

data Definition = Var {name :: String, value :: Expression}

instance Show Definition where
    show (Var _ e) = show e

data Form = Def Definition | Expr Expression deriving (Show)

data LispState = LispState [Definition]

class Display a where
    display :: a -> Int -> String

instance Display Constant where
    display (Number n) tab = replicate tab '\t' <> "Number: " <> (show n) <> "\n"
    display (Boolean b) tab = replicate tab '\t' <> "Boolean: " <> (show b) <> "\n"

instance Display Expression where
    display (ConstantExpression c) tab =
        replicate tab '\t'
        <> "ConstantExpression:\n" <> (display c (tab + 1))
    display (VariableExpression v) tab =
        replicate tab '\t'
        <> "VariableExpression:\n" <> (replicate (tab + 1) '\t') <> v <> "\n"
    display (CallExpresssion callee n) tab =
        replicate tab '\t'
        <> "Call:\n"
        <> (replicate (tab + 1) '\t')
        <> "Callee:\n"
        <> (display callee (tab + 2))
        <> (replicate (tab + 1) '\t')
        <> "Arguments:\n"
        <> (intercalate "" (map (\e -> display e (tab + 2)) n))
    display (Procedure n a body) tab =
        replicate tab '\t'
        <> "Procedure:\n"
        <> (replicate (tab + 1) '\t')
        <> "Name:\n" <> (replicate (tab + 2) '\t')
        <> n <> "\n" <> (replicate (tab + 1) '\t')
        <> "Arguments: (" <> (intercalate "," a) <>")\n"
        <> (replicate (tab + 1) '\t')
        <> "Body:\n"
        <> (intercalate "\n" (map (\e -> display e (tab + 2)) body))
    display (Builtin n _ _) tab =
        replicate tab '\t' <> "Builtin:\n" <> (replicate (tab + 1) '\t') <> n
    display (Lambda a body) tab =
        replicate tab '\t'
        <> "Lambda:\n"
        <> (replicate (tab + 1) '\t')
        <> "Arg: (" <> (intercalate "," a) <>")"
        <> (replicate (tab + 1) '\t')
        <> "Body:\n"
        <> (intercalate "\n" (map (\e -> display e (tab + 2)) body))
    display (If t c a) tab =
        replicate tab '\t'
        <> "If\n"
        <> (display t (tab + 1))
        <> (replicate tab '\t')
        <> "then\n"
        <> (display c (tab + 1))
        <> (replicate tab '\t')
        <> "else\n" <> (display a (tab + 1))

instance Display Definition where
    display (Var n e) tab =
        replicate tab '\t'
        <> "Define:\n"
        <> (replicate (tab + 1) '\t')
        <> "Name: " <> n <> "\n"
        <> (replicate (tab + 1) '\t')
        <> "Value:\n" <> (display e (tab + 2))

instance Display Form where
    display (Def d) tab =
        replicate tab '\t' <> "Definition:\n" <> (display d (tab + 1))
    display (Expr d) tab =
        replicate tab '\t' <> "Expression:\n" <> (display d (tab + 1))

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
initial = letter <|> special <|> oneOf "+-."

char :: StringParser Char
char = letter <|> digit <|> special <|> oneOf "+-."

constant :: StringParser Constant
constant =
    label "literal" $ (Number <$> integer)
    <|> (Boolean <$> ((True <$ string "#t") <|> (False <$ string "#f")))

identifier :: StringParser String
identifier =
    label "identifier" $ ((:) <$> initial <*> some Lisp.char)
    <|> pure
    <$> (letter <|> special <|> oneOf "+-")

arg :: StringParser [String]
arg = (many $ lexeme identifier)

call_pattern :: StringParser (String, [String])
call_pattern = label "call pattern" $ wrap $ (,) <$> lexeme identifier <*> arg

procedure :: StringParser (String, [String], [Expression])
procedure =
    label "procedure" $ (uncurry (,,) <$> call_pattern)
    <*> (some $ lexeme expression)

lambda :: StringParser ([String], [Expression])
lambda = label "procedure" $ (,) <$> (wrap arg) <*> (some $ lexeme expression)

expression :: StringParser Expression
expression = cexpr <|> vexpr <|> lamb <|> cond <|> callexpr
    where
        cexpr = ConstantExpression <$> constant
        vexpr = VariableExpression <$> identifier
        callexpr = wrap $ CallExpresssion <$> (lexeme expression)
            <*> many (lexeme expression)
        lamb = wrap $ (lexeme (string "lambda")) *> (uncurry Lambda <$> lambda)
        cond =  wrap $ If
            <$> ((lexeme (string "if")) *> (lexeme expression))
            <*> (lexeme expression) <*> (lexeme expression)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

_definition :: StringParser Definition
_definition = var <|> pro
    where
        var = promote
            <$> (lexeme (string "define") *> identifier)<*> (lexeme expression)
        pro = cast <$> (
                lexeme (string "define")
                *> (lexeme (uncurry3 Procedure <$> procedure)))
        cast p@(Procedure n _ _) = Var n p
        promote n (Lambda a body) = Var n (Procedure n a body)
        promote n e = Var n e

definition :: StringParser Definition
definition = wrap _definition

form :: StringParser Form
form = fdef <|> fexpr
    where
        fdef = Def <$> definition
        fexpr = Expr <$> expression

lispParse :: StringParser [Form]
lispParse = many (lexeme form)

builtinEq :: [Expression] -> LispState -> IO Expression
builtinEq lst s =
    (ConstantExpression . Boolean)
    <$> ((==) <$> (collect (lst!!0) s)
    <*> (collect (lst!!1) s))

builtinLower :: [Expression] -> LispState -> IO Expression
builtinLower lst s = low <$> (collect (lst!!0) s >>= convert)
    <*> (collect (lst!!1) s >>= convert)
    where
        low (ConstantExpression (Number a)) (ConstantExpression (Number b)) =
            (ConstantExpression . Boolean) $ a < b
        convert c@(ConstantExpression (Boolean _)) =
            fail $ show c <> " is not a real number"
        convert c@(ConstantExpression _) = pure c
        convert d = fail $ show d <> " is not a real number"

builtinAdd :: [Expression] -> LispState -> IO Expression
builtinAdd lst s = add <$> (collect (lst!!0) s >>= convert)
    <*> (collect (lst!!1) s >>= convert)
    where
        add (ConstantExpression (Number a)) (ConstantExpression (Number b)) =
            (ConstantExpression . Number) $ a + b
        convert c@(ConstantExpression (Boolean _)) =
            fail $ show c <> " is not a number"
        convert c@(ConstantExpression _) = pure c
        convert p = fail $ show p <> " is not a number"

builtinSub :: [Expression] -> LispState -> IO Expression
builtinSub lst s = sub <$> (collect (lst!!0) s >>= convert)
    <*> (collect (lst!!1) s >>= convert)
    where
        sub (ConstantExpression (Number a)) (ConstantExpression (Number b)) =
            (ConstantExpression . Number) $ a - b
        convert c@(ConstantExpression (Boolean _)) =
            fail $ show c <> " is not a number"
        convert c@(ConstantExpression _) = pure c
        convert d = fail $ show d <> " is not a number"

builtinMul :: [Expression] -> LispState -> IO Expression
builtinMul lst s = mul <$> (collect (lst!!0) s >>= convert)
    <*> (collect (lst!!1) s >>= convert)
    where
        mul (ConstantExpression (Number a)) (ConstantExpression (Number b)) =
            (ConstantExpression . Number) $ a * b
        convert c@(ConstantExpression (Boolean _)) =
            fail $ show c <> " is not a number"
        convert c@(ConstantExpression _) = pure c
        convert p = fail $ show p <> " is not a number"

builtinDiv :: [Expression] -> LispState -> IO Expression
builtinDiv lst s = di <$> (collect (lst!!0) s >>= convert)
    <*> (collect (lst!!1) s >>= convert >>= pass)
    where
        di (ConstantExpression (Number a)) (ConstantExpression (Number b)) =
            (ConstantExpression . Number) $ div a b
        convert c@(ConstantExpression (Number _)) = pure c
        convert p = fail $ show p <> " is not a number"
        pass c@(ConstantExpression (Number n)) =
            (guard (n == 0) >> pure c) <|> fail "undefined for 0"

builtinMod :: [Expression] -> LispState -> IO Expression
builtinMod lst s =  mo <$> (collect (lst!!0) s >>= convert)
    <*> (collect (lst!!1) s >>= convert >>= pass)
    where
        mo (ConstantExpression (Number a)) (ConstantExpression (Number b)) =
            (ConstantExpression . Number) $ mod a b
        convert c@(ConstantExpression (Number n)) = pure c
        convert c@(ConstantExpression _) = pure c
        convert p = fail $ show p <> " is not a number"
        pass c@(ConstantExpression (Number n)) =
            (guard (n == 0) >> pure c) <|> fail "undefined for 0"

defaultState :: LispState
defaultState = LispState [
    (Var "eq?" (Builtin "eq?" 2 builtinEq)),
    (Var "<" (Builtin "<" 2 builtinLower)),

    (Var "+" (Builtin "+" 2 builtinAdd)),
    (Var "-" (Builtin "-" 2 builtinSub)),
    (Var "*" (Builtin "*" 2 builtinMul)),
    (Var "div" (Builtin "div" 2 builtinDiv)),
    (Var "mod" (Builtin "mod" 2 builtinMod))
    ]

test :: Expression -> LispState -> IO Bool
test (ConstantExpression (Boolean b)) _ = pure b
test (CallExpresssion e@(Procedure _ _ _) args) s = eval =<< (collect e s)
    where
        eval (Procedure _ a body) = 
            (evaluateBody body =<< generateProcedureScope (zip a args) s)
            >>= (\ex -> Lisp.test ex s)
test (CallExpresssion e args) s = eval =<< (collect e s)
    where
        eval (Lambda a body) =
            (evaluateBody body =<< generateProcedureScope (zip a args) s)
            >>= (\ex -> Lisp.test ex s)
        eval (Builtin _ narg func) = guard (length args == narg)
            *>((\ex -> collect ex s)=<<(func args s))>>= (\e -> Lisp.test e s)
        eval ex = fail $ "attempt to apply non-procedure " <> show ex
test e _ = print e *> pure True

collect :: Expression -> LispState -> IO Expression
collect (VariableExpression n) s@(LispState defs) = 
    (chute =<< (mfromMaybe =<< searchDefinition n defs))
    <|> fail ("variable " <> n <> " is not bound")
    where
        chute (Var _ val) = collect val s
collect (CallExpresssion e args) s@(LispState _) = eval =<< (collect e s)
    where
        eval (Procedure _ a body) = 
            (evaluateBody body =<< generateProcedureScope (zip a args) s)
        eval (Builtin _ narg func) =
            guard (length args == narg)
            *> ((\ex -> collect ex s) =<< (func args s))
        eval (Lambda a body) = 
            (evaluateBody body =<< generateProcedureScope (zip a args) s)
        eval ex = fail $ "attempt to apply non-procedure " <> show ex
collect (If t c a) s@(LispState _) = condition =<< (Lisp.test t s)
    where
        condition True = collect c s
        condition False = collect a s
collect e _ = pure e

addDefinition :: Definition -> [Definition] -> IO [Definition]
addDefinition def [] = pure [def]
addDefinition def (x:xs) =
    ((def:xs) <$ guard (name def == name x))
    <|> ((x:) <$> addDefinition def xs)

addDefinitions :: [Definition] -> [Definition] -> IO [Definition]
addDefinitions [] defs = pure defs
addDefinitions (x:xs) defs = addDefinition x defs >>= addDefinitions xs

searchDefinition :: String -> [Definition] -> IO (Maybe Definition)
searchDefinition _ [] = pure $ Nothing
searchDefinition n (x:xs) =
    (pure x) <$ guard (name x == n) <|> searchDefinition n xs

generateProcedureScope :: [(String, Expression)] -> LispState -> IO LispState
generateProcedureScope lst s@(LispState def) =
    (pure . LispState) =<< add =<< (mapM (uncurry coll) lst)
    where
        coll n e = Var n <$> (collect e s)
        add l = addDefinitions l def

evaluateBody :: [Expression] -> LispState -> IO Expression
evaluateBody [] _ = fail "No body"
evaluateBody [x] s = collect x s
evaluateBody (x:xs) s = evaluateExpressionInBody x s >>= (evaluateBody xs)

evaluateExpressionInBody :: Expression -> LispState -> IO LispState
evaluateExpressionInBody (VariableExpression n) s@(LispState defs) =
    (
        disp =<< (mfromMaybe =<< searchDefinition n defs)
    ) <|> fail ("variable " <> n <> " is not bound")
    where
        disp (Var _ val) = evaluateExpressionInBody val s
evaluateExpressionInBody (CallExpresssion e args) s@(LispState _) =
    eval =<< (collect e s)
    where
        eval (Procedure _ a body) = (evaluateBody body
                =<< generateProcedureScope (zip a args) s) *> pure s
        eval (Builtin _ narg func) = guard (length args == narg)
            *> ((\ex -> evaluateExpressionInBody ex s)=<<(func args s))*>pure s
        eval (Lambda a body) = (evaluateBody body
                =<< generateProcedureScope (zip a args) s) *> pure s
        eval ex = fail $ "attempt to apply non-procedure " <> show ex
evaluateExpressionInBody _ s = pure s

evaluateExpression :: Expression -> LispState -> IO LispState
evaluateExpression (VariableExpression n) s@(LispState defs) =
    (
        disp
        =<< (mfromMaybe =<< searchDefinition n defs)
    ) <|> fail ("variable " <> n <> " is not bound")
    where
        disp (Var _ val) = evaluateExpression val s
evaluateExpression (CallExpresssion e args) s@(LispState _) =
    eval =<< (collect e s)
    where
        eval (Procedure _ a body) =((evaluateBody body
                =<< generateProcedureScope (zip a args) s) >>= print) *> pure s
        eval (Builtin _ narg func) = guard (length args == narg)
            *> ((\ex -> evaluateExpression ex s) =<< (func args s)) *> pure s
        eval (Lambda a body) = ((evaluateBody body
                =<< generateProcedureScope (zip a args) s) >>= print) *> pure s
        eval ex = fail $ "attempt to apply non-procedure " <> show ex
evaluateExpression (If t c a) s =
    (condition =<< (sink =<< (collect t s))) *> pure s
    where
        sink ex = Lisp.test ex s
        condition True = evaluateExpression c s
        condition False = evaluateExpression a s
evaluateExpression f s = print f *> pure s

evaluate :: Form -> LispState -> IO LispState
evaluate (Def (Var n e)) s@(LispState lst) =
    LispState
    <$> (((Var n) <$> (collect e s))
    >>= (\def -> addDefinition def lst))
evaluate (Expr e) state = evaluateExpression e state

lispInterpreter :: [Form] -> LispState -> IO LispState
lispInterpreter [] s = pure s
lispInterpreter (x:xs) state = evaluate x state >>= (lispInterpreter xs)

lispFileInterpreter :: IO ()
lispFileInterpreter =
        (getContents >>= (pure . snd . (runParse lispParse))) >>= result
    where
        result (Right forms) = () <$ lispInterpreter forms defaultState
        result (Left es) = (putStrLn . show) es

queryLine :: Bool -> LispState -> String -> IO LispState
queryLine True state ":quit" = _lispCLI False state
queryLine True state ":q" = _lispCLI False state
queryLine False state ":debug" = _lispCLI True state
queryLine False state ":d" = _lispCLI True state
queryLine d state l = ((result d state) . snd) $ runParse lispParse l

result :: Bool -> LispState -> Either [Message String] [Form] -> IO LispState
result debug state (Right forms) =
    (guard debug >> r) <|> l
    where
        r = putStrLn (intercalate "   " (map (\e -> display e 0) forms))
            >> (_lispCLI True =<< handle =<< (exec forms))
        l = _lispCLI False =<< handle =<< (exec forms)
result debug state (Left es) = _lispCLI debug state <* (putStrLn . show) es

handle :: LispState -> Either SomeException LispState -> IO LispState
handle state (Left e) = print e *> pure state
handle state (Right s) = pure s

exec :: LispState -> [Form] -> IO (Either SomeException LispState)
exec state forms = Control.Exception.try $
    lispInterpreter forms state

_lispCLI :: Bool -> LispState -> IO LispState
_lispCLI True state =
        putStr "debug> " *> (hFlush stdout) *> (getLine >>= queryLine)
_lispCLI False state =
    putStr "> " *> (hFlush stdout) *> (getLine >>= queryLine)

lispCLI :: IO ()
lispCLI = (() <$ _lispCLI False defaultState) <|> (putStrLn "")
