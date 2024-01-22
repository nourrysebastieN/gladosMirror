
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Dawn where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Functor

import Monoparsec
import qualified Monoparsec.Message as Msg

import Option

type Parser a = Parsec String CompilerOption a

data Literal
    = Boolean Bool
    | Integer Int
    | Str String
    | Character Char
    | Identifier String
    | Structure (Tok String) [Tok Expression]
    | Oblivion
    deriving (Show)

data Type
    = Simple (Tok String)
    | Complex (Tok String) [Tok Type]
    deriving (Show)

type FunctionEntry = ([Tok Literal], (Tok Expression))

data Expression
    = Literal (Tok Literal)
    | Call (Tok String) [Tok Expression]
    | Fold (Tok Expression)
    deriving (Show)

data Declaration
    = Struct (Tok String) [Tok String] [Tok ((Tok String), [Tok String])]
    | Function (Tok String) [Tok Type] (Tok Type)
    | FunctionSpec (Tok String) [Tok FunctionEntry]
    deriving (Show)

newtype Program = Program [Tok Declaration]
    deriving (Show)

binary :: Parser Char
binary = like '0' <|> like '1'

a :: Parser Char
a = binary *> scn *> binary

octal :: Parser Char
octal = satisfy Data.Char.isOctDigit

hexa :: Parser Char
hexa = satisfy Data.Char.isHexDigit

integerLiteral :: Parser Literal
integerLiteral = getOffset >>= func
    where
        func s = Integer <$> (bin s <|> oct s <|> hex s <|> fallback (\e _ -> range (Msg.Range s e) (Monoparsec.error "invalid decimal literal")) Monoparsec.integer)

        binHead = Monoparsec.string "0b" <|> Monoparsec.string "0B"
        bin s = num 2 <$> (binHead *> fallback (\e _ -> range (Msg.Range s e) (fail "invalid binary literal")) (some binary <* tail))

        octHead = Monoparsec.string "0o" <|> Monoparsec.string "0O"
        oct s = num 8 <$> (octHead *> fallback (\e _ -> range (Msg.Range s e) (fail "invalid octal literal")) (some octal <* tail))

        hexHead = Monoparsec.string "0x" <|> Monoparsec.string "0X"
        hex s = num 16 <$> (hexHead *> fallback (\e _ -> range (Msg.Range s e) (fail "invalid hexadecimal literal")) (some hexa <* tail))

        tail = void (ahead (satisfy (not . Data.Char.isAlphaNum) :: Parser Char)) <|> eof

        num b = Data.Foldable.foldl' (step b) 0
        step b a c = a * b + Data.Char.digitToInt c

booleanLiteral :: Parser Literal
booleanLiteral = Boolean <$> ((Monoparsec.string "True" $> True) <|> (Monoparsec.string "False" $> False)) <|> Monoparsec.error "invalid boolean literal"

stringLiteral :: Parser Literal
stringLiteral = getOffset >>= func
    where
        func s = Str <$> (like '"' *> fallback (extract s) (manyUntil char (like '"')))

        extract s e msg = net msg (Msg.getReason msg) s e
        net _ (Expectation _ _) s e = suggest [Msg.info (Single s) (Message "string literal begin here")] $ range (Msg.Range s e) $ fail "untermined string literal"
        net m _ _ _ = Monoparsec.error' m

charLiteral :: Parser Literal
charLiteral = getOffset >>= func
    where
        func s = Character <$> (like '\'' *> (fallback (control s) char <* fallback (net s) (like '\'')))

        control _ e _ = range (Msg.Range e e) (fail "invalid character literal")

        net s e m = net' m (Msg.getReason m) s e
        net' _ (Expectation _ _) s e = range (Msg.Range s e) (fail "untermined character literal")
        net' m _ _ _ = Monoparsec.error' m

identifierLiteral :: Parser Literal
identifierLiteral = Identifier <$> identifier

structLiteral' :: Parser Literal
structLiteral' = Structure <$> (tokenize typename) <*> (many parameter)
    where
        parameter = sc *> (tokenize expression)

structLiteral :: Int -> Parser Literal
structLiteral s =
    like '('
    *> sc *> structLiteral'
    <* sc <* fallback (end s) (like ')')
    where
        end s e _ = suggest
            [Msg.info (Single s) (Message "'(' here")]
            $ range (Single e) $ fail "missing closing ')'"

oblivion :: Parser Literal
oblivion = Oblivion <$ (like '_')

emptyStruct :: Parser Literal
emptyStruct = Structure <$> (tokenize typename) <*> pure []

literal :: Parser Literal
literal = integerLiteral
    <|> booleanLiteral
    <|> stringLiteral
    <|> charLiteral
    <|> identifierLiteral
    <|> (getOffset >>= structLiteral)
    <|> emptyStruct
    <|> oblivion

typename :: Parser String
typename = (:) <$> (satisfy Data.Char.isUpper)
    <*> many (satisfy Data.Char.isAlphaNum <|> like '_')

identifier :: Parser String
identifier = (:) <$> (satisfy Data.Char.isLower)
    <*> many (satisfy Data.Char.isAlphaNum <|> like '_')

endLine :: Parser ()
endLine = void $ manyUntil (satisfy Data.Char.isSpace) (satisfy (== '\n'))

sc :: Parser ()
sc = void $ many (like ' ')

scn :: Parser ()
scn = void $ many (like ' ') *> (void (like '\n') <|> eof)

indent :: Int -> Parser Int
indent i = Monoparsec.lookup check $ while (== ' ')
    where
        check s = if ((>= i) . length) s then pure (length s) else getOffset >>= err
        err i = range (All i) $ Monoparsec.error "invalid indentation"

noIndent :: Parser ()
noIndent = void $ Monoparsec.lookup check $ while (== ' ')
    where
        check s = if null s then pure () else getOffset >>= err
        err i = range (All i) $ Monoparsec.error "invalid indentation"

consumer :: Parser ()
consumer = void $ manyUntil ((satisfy Data.Char.isAlphaNum) <|> like '_') (ahead ((void $ satisfy Data.Char.isSpace) <|> eof))

stringError :: String -> Parser a -> Parser a
stringError msg p = fallback (err msg) p
    where
        err msg i _ = isEof >>= (\b -> consumer *> getOffset >>= (\e -> output msg i (e - 1) b))
        output msg s e b = if b then range (Msg.Range s e) $ Monoparsec.error msg else range (Msg.Range s e) $ fail msg

fullType :: Parser Type
fullType = Monoparsec.lookup mutate $ Complex <$> (tokenize $ typename <|> identifier) <*> parameters
    where
        func s = (Simple <$> (tokenize $ typename <|> identifier)) <|> (like '(' *> sc *> fullType <* sc <* fallback (end s) (like ')'))

        end s e _ = suggest [Msg.info (Single s) (Message "'(' here")] $ range (Single e) $ fail "missing closing ')'"

        parameters = many parameter
        parameter = sc *> tokenize (getOffset >>= func)

        mutate (Complex t []) = pure $ Simple t
        mutate t = pure t

struct :: Parser Declaration
struct = Struct <$> (name <* sc) <*> fields <*> block
    where
        func s o p = fallback (err' s o) p
        err' str s e _ = range (Msg.Range s e) $ fail str

        err msg i _ = isEof >>= (\b -> consumer *> getOffset >>= (\e -> output msg i (e - 1) b))
        output msg s e b = if b then range (Msg.Range s e) $ Monoparsec.error msg else range (Msg.Range s e) $ fail msg

        head = noIndent *> Monoparsec.string "struct" <* sc
        name = head *> (tokenize $ stringError "expected a typename" typename)

        fields = many field
        field = sc *> ahead (unlike '\n') *> (tokenize $ stringError "expected a identifier" identifier)

        blockHead = (sc *> like '{' <* scn) <|> (scn *> noIndent *> like '{' <* scn)
        block = blockHead *> (many (tokenize constructor)) <* (noIndent *> like '}' <* scn)
        constructor = (,) <$> (indent 1 *> (tokenize $ stringError "expected a typename" typename)) <*> fields <* scn

function :: Parser Declaration
function = (Function <$> name) >>= func
    where
        name = noIndent *> (tokenize $ stringError "expected a identifier" identifier) <* sc <* Monoparsec.string "-"

        func f = parameters >>= (\l -> fmap (uncurry f) (alt l))
 
        alt [x] = 
            let f (Left _) = pure ([], x) <* scn
                f (Right _) = ([x],) <$> ret
            in secure (ahead (sc *> Monoparsec.string "->")) >>= f
        alt l = (l,) <$> ret
        parameters = (:) <$> (sc *> (tokenize fullType)) <*> many (sc *> like ',' *> sc *> (tokenize fullType))
        ret = sc *> Monoparsec.string "->" *> sc *> (tokenize fullType) <* scn

functionEntry :: Int -> Parser FunctionEntry
functionEntry n =(,) <$> matches <*> result
    where
        matches = many (sc *> (tokenize literal))
        result = sc *> like '=' *> alt
        alt = (scn *> (absolute $ indent (n + 1)) *> (tokenize headExpression) <* scn) <|> (sc *> (tokenize headExpression) <* scn)

functionSpec :: Parser Declaration
functionSpec = FunctionSpec <$> (noIndent *> (tokenize $ stringError "expected a identifier" identifier)) <* sc <*> block
    where
        blockHead = (sc *> like '{' <* scn) <|> (scn *> noIndent *> like '{' <* scn)
        block = blockHead *> (indent 1 >>= (\i -> many (tokenize $ functionEntry i))) <* (noIndent *> like '}' <* scn)

operator :: Parser String
operator = getOffset >>= func
    where
        func s = (((:) <$> (oneOf "!@#$%^&*-+=|\\:<>/?~") <*> many (oneOf "!@#$%^&*-+=|\\:<>/?~")) >>= (check s))
        err s e = range (Msg.Range s (e - 1)) $ Monoparsec.error "invalid operator"

        check s "=" = getOffset >>= (err s)
        check s "?" = getOffset >>= (err s)
        check s ":" = getOffset >>= (err s)
        check s "@" = getOffset >>= (err s)
        check _ s = pure s

expression :: Parser Expression
expression = mutate =<< ((getOffset >>= fold) <|> leaf)
    where
        mutate e = pure e
        
        leaf = Literal <$> tokenize literal

        fold s = foldOp s <|> foldExp s
        foldOp s = Call <$> ((like '(') *> sc *> (tokenize operator) <* sc <* fallback (end s) (like ')')) <*> (many (sc *> (tokenize expression)))
        foldExp s = Fold <$> ((like '(') *> sc *> (tokenize headExpression) <* sc <* fallback (end s) (like ')'))
        end s e _ = suggest [Msg.info (Single s) (Message "'(' here")] $ range (Single e) $ fail "missing closing ')'"

headExpression :: Parser Expression
headExpression = mutate =<< (tokenize $ ((getOffset >>= fold) <|> leaf))
    where
        mutate :: Tok Expression -> Parser Expression
        mutate (Tok i e (Literal (Tok i' e' (Identifier s)))) = (secure (sc *> (tokenize operator))) >>= (sink i e (Tok i' e' s))
        mutate (Tok i e (Literal (Tok _ _ (Structure s [])))) = Literal <$> (Tok i e <$> (Structure s <$> (many (sc *> (tokenize expression)))))
        mutate e = (secure (sc *> (tokenize operator))) >>= (fall e)
        
        tok s v e = Tok s e v

        sink i e s r = case r of
            Left _ -> mutate =<< (tok i <$> (Call s <$> (many (sc *> (tokenize expression)))) <*> getOffset)
            Right o -> op (Tok i e (Literal (Identifier <$> s))) o
        
        fall :: Tok Expression -> Either (Msg.Message String) (Tok String) -> Parser Expression
        fall e r = case r of
            Left _ -> case e of
                (Tok _ _ (Call s [])) -> pure $ Literal (Identifier <$> s)
                _ -> pure $ fromToken e
            Right s -> op e s
        
        op e o = Call o <$> ((e:) <$> ((:[]) <$> (sc *> (tokenize headExpression))))
        leaf = Literal <$> tokenize literal

        fold s = foldOp s <|> foldExp s
        foldOp s = Call <$> ((like '(') *> sc *> (tokenize operator) <* sc <* fallback (end s) (like ')')) <*> (many (sc *> (tokenize expression)))
        foldExp s = Fold <$> ((like '(') *> sc *> (tokenize headExpression) <* sc <* fallback (end s) (like ')'))
        end s e _ = suggest [Msg.info (Single s) (Message "'(' here")] $ range (Single e) $ fail "missing closing ')'"

prog :: Parser [Tok Declaration]
prog = manyUntil decl eof
    where
        decl = (many scn) *> tokenize (struct <|> function <|> functionSpec)

-- checkIdentifier :: Parser String -> Parser String
-- checkIdentifier id = Monoparser.lookup func id
--     where
--         func s =
--             case s of
--                "if" -> fail "invalid identifier"
--                "else" -> fail "invalid identifier"
--                "while" -> fail "invalid identifier"
--                "return" -> fail "invalid identifier"
--                _ -> pure s

-- identifier :: Parser String
-- identifier = checkIdentifier ((:) <$> (satisfy Data.Char.isAlpha <|> like '_' ) <*> many (satisfy Data.Char.isAlphaNum <|> like '_'))



-- operator :: Parser String
-- operator =
--     Monoparsec.string "=="
--     <|> Monoparsec.string "/="
--     <|> Monoparsec.string ">="
--     <|> Monoparsec.string "<="
--     <|> Monoparsec.string ">"
--     <|> Monoparsec.string "<"
--     <|> Monoparsec.string "+"
--     <|> Monoparsec.string "-"
--     <|> Monoparsec.string "*"
--     <|> Monoparsec.string "/"
--     <|> Monoparsec.string "%"

-- variable :: (Tok Expression) -> Parser (Tok Expression)
-- variable (fromToken -> (Identifier s)) = tokenize (Var <$> (s <$ whitespace <* like '=' <* whitespace) <*> expression)
-- variable _ = Monoparsec.error "invalid identifier"

-- arith :: (Tok Expression) -> Parser (Tok Expression)
-- arith e = tokenize (Arith e <$> (whitespace *> operator) <*> (whitespace *> expression))

-- leafExpression :: Parser (Tok Expression)
-- leafExpression = tokenize ((Literal <$> literal) <|> (Identifier <$> identifier))

-- mutateExpression :: Tok Expression -> Parser (Tok Expression)
-- mutateExpression e = fallback (\_ _ -> pure e) (arith e <|> variable e)

-- expression :: Parser (Tok Expression)
-- expression = mutateExpression =<< leafExpression <|> (getOffset >>= func)
--     where
--         func s = tokenize (Fold <$> fold s)

--         head = like '('
--         tail = like ')'
--         fold s = head *> whitespace *> overload (net s) (Monoparsec.maybe expression) <* whitespace <* fallback (end s) tail

--         net s e m = net' (Msg.getReason m) s e
--         net' (Expectation _ _) s e = Msg.error (Msg.Range (s + 1) e) (Message "invalid expression")
--         net' (Message m) s e = Msg.error (Msg.Range (s + 1) e) (Message ("invalid expression: " <> m))

--         end s e _ = suggest [Msg.info (Single s) (Message "'(' here")] $ range (Single e) $ fail "missing closing ')'"

-- block :: Int -> Parser [Tok Statement]
-- block = fold
--     where
--         fold s = fallback (start s) (like '{') *> whitespace *> overload (net s) (many statement) <* whitespace <* fallback (end s) (like '}')
        
--         net s e m = net' (Msg.getReason m) s e
--         net' (Expectation _ _) s e = Msg.error (Msg.Range (s + 1) e) (Message "invalid statement")
--         net' (Message m) s e = Msg.error (Msg.Range (s + 1) e) (Message ("invalid statement: " <> m))

--         start s e _ = range (Single e) $ fail "expected '{' at end of input"
--         end s e _  = suggest [Msg.info (Single s) (Message "'{' here")] $ range (Single e) $ fail "missing closing '}'"

-- elseStatement :: Parser (Tok Statement)
-- elseStatement = (whitespace *> Monoparsec.string "else" <* whitespace *> promote)
--     where
--         promote = ifStatement <|> tokenize (ElseStatement <$> (getOffset >>= block))

-- ifStatement :: Parser (Tok Statement)
-- ifStatement = tokenize (IfStatement <$> (Monoparsec.string "if" *> whitespace *> expression <* whitespace) <*> (getOffset >>= block) <*> Monoparsec.maybe elseStatement)

-- whileStatement :: Parser (Tok Statement)
-- whileStatement = tokenize (WhileStatement <$> (Monoparsec.string "while" *> whitespace *> expression <* whitespace) <*> (getOffset >>= block))

-- returnStatement :: Parser (Tok Statement)
-- returnStatement = tokenize (ReturnStatement <$> (Monoparsec.string "return" *> whitespace *> Monoparsec.maybe expression))

-- statement :: Parser (Tok Statement) 
-- statement = whitespace *> (ifStatement <|> whileStatement <|> returnStatement <|> tokenize (Expression <$> expression))

-- program :: Parser (Tok Program)
-- program = tokenize (Program <$> (many (whitespace *> statement <* whitespace) <* eof))

-- test :: Parser Bool
-- test = Monoparsec.lookup func (Monoparsec.string "TRUE" <|> Monoparsec.string "FALSE")
--     where
--         func s | s == "TRUE" = pure True
--                 | otherwise = pure False