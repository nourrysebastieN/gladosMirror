
{-# LANGUAGE ViewPatterns #-}

module Twillight where

import Data.Word
import qualified Data.List
import Data.Maybe
import Data.Int
import Data.ByteString
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Control.Applicative
import Control.Exception

import Debug.Trace

import qualified Dawn as D
import Dusk
import Monoparsec (Tok(..), fromToken)

data Typee
    = Boolean Bool
    | Integer Int
    | Str String
    | Character Char
    | Struct Int Int [Typee]
    | Adress Int
    deriving (Show, Eq)

data Instruction
    = Noop
    
    | PushBool
    | PushInt
    | PushStr
    | PushChar
    | PushStruct
    | PushAdress
    
    | Extract
    
    | Pop
    | PopN
    
    | Copy

    | Call
    | Ret

    | Load
    | End
    
    | Crash
    | Put
    | Print

    | Add
    | Sub
    | Mul
    | Div
    | Mod

    | Eq
    | Neq
    | Lt
    | Gt
    | Leq
    | Geq

    | Or
    | And
    | Not
    | Xor

    | Show
    deriving (Show, Eq)

data VMState = VMState
    { 
        stack :: [Typee],
        rip :: Int
    } deriving (Show, Eq)

class Compiler a where
    compile' :: a -> [Word8]

instance Compiler D.Literal where
    compile' (D.Boolean b) = [fromIntegral (fromEnum PushBool), fromIntegral (fromEnum b)]
    compile' (D.Integer i) = [fromIntegral (fromEnum PushInt), fromIntegral (fromEnum (i `Prelude.div` 16777216)), fromIntegral (fromEnum (i `Prelude.div` 65536)), fromIntegral (fromEnum (i `Prelude.div` 256)), fromIntegral (fromEnum i)]
    compile' (D.Str s) = [fromIntegral (fromEnum PushStr)] ++ (Prelude.map (fromIntegral . fromEnum) s) ++ [0]
    compile' (D.Character c) = [fromIntegral (fromEnum PushChar), fromIntegral (fromEnum c)]

instance Compiler D.Expression where
    compile' (D.Literal l) = (compile' . fromToken) l
    compile' (D.Call o a) = (Prelude.concat $ Prelude.map (compile' . fromToken) a) ++ ((op . fromToken) o)
    compile' (D.Fold e) = (compile' . fromToken) e

op :: String -> [Word8]
op "+" = [fromIntegral (fromEnum Add)]
op "-" = [fromIntegral (fromEnum Sub)]
op "*" = [fromIntegral (fromEnum Mul)]
op "div" = [fromIntegral (fromEnum Div)]
op "mod" = [fromIntegral (fromEnum Mod)]
op "eq" = [fromIntegral (fromEnum Eq)]
op "neq" = [fromIntegral (fromEnum Neq)]
op "<" = [fromIntegral (fromEnum Lt)]
op ">" = [fromIntegral (fromEnum Gt)]
op "<=" = [fromIntegral (fromEnum Leq)]
op ">=" = [fromIntegral (fromEnum Geq)]
op "==" = [fromIntegral (fromEnum Eq)]
op "!=" = [fromIntegral (fromEnum Neq)]
op "||" = [fromIntegral (fromEnum Or)]
op "&&" = [fromIntegral (fromEnum And)]
op "not" = [fromIntegral (fromEnum Not)]
op "!|" = [fromIntegral (fromEnum Xor)]
op "print" = [fromIntegral (fromEnum Print)]
op "put" = [fromIntegral (fromEnum Put)]
op "error" = [fromIntegral (fromEnum Crash)]
op "show" = [fromIntegral (fromEnum Show)]
op _ = []

instance Enum Instruction where
    fromEnum Noop = 0x00
    fromEnum PushBool = 0x01
    fromEnum PushInt = 0x02
    fromEnum PushStr = 0x03
    fromEnum PushChar = 0x04
    fromEnum PushStruct = 0x05
    fromEnum PushAdress = 0x06
    fromEnum Extract = 0x07
    fromEnum Pop = 0x08
    fromEnum PopN = 0x09
    fromEnum Copy = 0x0a
    fromEnum Call = 0x0b
    fromEnum Ret = 0x0c
    fromEnum Load = 0x0d
    fromEnum End = 0x0e
    fromEnum Crash = 0x0f
    fromEnum Put = 0x10
    fromEnum Print = 0x11
    fromEnum Add = 0x12
    fromEnum Sub = 0x13
    fromEnum Mul = 0x14
    fromEnum Div = 0x15
    fromEnum Mod = 0x16
    fromEnum Eq = 0x17
    fromEnum Neq = 0x18
    fromEnum Lt = 0x19
    fromEnum Gt = 0x1a
    fromEnum Leq = 0x1b
    fromEnum Geq = 0x1c
    fromEnum Or = 0x1d
    fromEnum And = 0x1e
    fromEnum Not = 0x1f
    fromEnum Xor = 0x20
    fromEnum Show = 0x21

    toEnum 0x00 = Noop
    toEnum 0x01 = PushBool
    toEnum 0x02 = PushInt
    toEnum 0x03 = PushStr
    toEnum 0x04 = PushChar
    toEnum 0x05 = PushStruct
    toEnum 0x06 = PushAdress
    toEnum 0x07 = Extract
    toEnum 0x08 = Pop
    toEnum 0x09 = PopN
    toEnum 0x0a = Copy
    toEnum 0x0b = Call
    toEnum 0x0c = Ret
    toEnum 0x0d = Load
    toEnum 0x0e = End
    toEnum 0x0f = Crash
    toEnum 0x10 = Put
    toEnum 0x11 = Print
    toEnum 0x12 = Add
    toEnum 0x13 = Sub
    toEnum 0x14 = Mul
    toEnum 0x15 = Div
    toEnum 0x16 = Mod
    toEnum 0x17 = Eq
    toEnum 0x18 = Neq
    toEnum 0x19 = Lt
    toEnum 0x1a = Gt
    toEnum 0x1b = Leq
    toEnum 0x1c = Geq
    toEnum 0x1d = Or
    toEnum 0x1e = And
    toEnum 0x1f = Not
    toEnum 0x20 = Xor
    toEnum 0x21 = Show

add' :: Typee -> Typee -> IO Typee
add' (Integer a) (Integer b) = pure (Integer (a + b))
add' _ _ = fail "error: Add: Invalid argument"

sub' :: Typee -> Typee -> IO Typee
sub' (Integer a) (Integer b) = pure (Integer (a - b))
sub' _ _ = fail "error: Sub: Invalid argument"

mul' :: Typee -> Typee -> IO Typee
mul' (Integer a) (Integer b) = pure (Integer (a * b))
mul' _ _ = fail "error: Mul: Invalid argument"

div' :: Typee -> Typee -> IO Typee
div' (Integer a) (Integer b) = when (b == 0) (fail "error: Div: divide by 0") >> (pure (Integer (a `Prelude.div` b)))
div' _ _ = fail "error: Div: Invalid argument"

mod' :: Typee -> Typee -> IO Typee
mod' (Integer a) (Integer b) = when (b == 0) (fail "error: Mod: divide by 0") >> (pure (Integer (a `Prelude.mod` b)))
mod' _ _ = fail "error: Mod: Invalid argument"

eq' :: Typee -> Typee -> IO Typee
eq' (Integer a) (Integer b) = pure (Boolean (a == b))
eq' (Boolean a) (Boolean b) = pure (Boolean (a == b))
eq' _ _ = fail "error: Eq: Invalid argument"

neq' :: Typee -> Typee -> IO Typee
neq' (Integer a) (Integer b) = pure (Boolean (a /= b))
neq' (Boolean a) (Boolean b) = pure (Boolean (a /= b))
neq' _ _ = fail "error: Neq: Invalid argument"

lt' :: Typee -> Typee -> IO Typee
lt' (Integer a) (Integer b) = pure (Boolean (a < b))
lt' _ _ = fail "error: Lt: Invalid argument"

gt' :: Typee -> Typee -> IO Typee
gt' (Integer a) (Integer b) = pure (Boolean (a > b))
gt' _ _ = fail "error: Gt: Invalid argument"

leq' :: Typee -> Typee -> IO Typee
leq' (Integer a) (Integer b) = pure (Boolean (a <= b))
leq' _ _ = fail "error: Leq: Invalid argument"

geq' :: Typee -> Typee -> IO Typee
geq' (Integer a) (Integer b) = pure (Boolean (a >= b))
geq' _ _ = fail "error: Geq: Invalid argument"

or' :: Typee -> Typee -> IO Typee
or' (Boolean a) (Boolean b) = pure (Boolean (a || b))
or' _ _ = fail "error: Or: Invalid argument"

and' :: Typee -> Typee -> IO Typee
and' (Boolean a) (Boolean b) = pure (Boolean (a && b))
and' _ _ = fail "error: And: Invalid argument"

not' :: Typee -> IO Typee
not' (Boolean a) = pure (Boolean (Prelude.not a))
not' _ = fail "error: Not: Invalid argument"

xor_ :: Bool -> Bool -> Bool
xor_ a b = (a || b) && (Prelude.not (a && b))

xor' :: Typee -> Typee -> IO Typee
xor' (Boolean a) (Boolean b) = pure (Boolean (a `xor_` b))

pushBool :: VMState -> [Word8] -> IO VMState
pushBool state [] = fail "error: PushBool: Missing argument"
pushBool state (x:_) = pure (push (Boolean c) (next 1 state))
    where
        c = if (fromIntegral x :: Int) > 0 then True else False 

pushInt :: VMState -> [Word8] -> IO VMState
pushInt state [] = fail "error: PushInt: Missing argument"
pushInt state (x:y:z:w:_) = pure (push (Integer n) (next 4 state))
    where
        n = (fromIntegral (fromIntegral x :: Int8) :: Int) * 16777216 + (fromIntegral y :: Int) * 65536 + (fromIntegral z :: Int) * 256 + (fromIntegral w :: Int)
pushInt state _ = fail "error: PushInt: Invalid argument"

pushStr :: VMState -> [Word8] -> IO VMState
pushStr state [] = fail "error: PushStr: Missing argument"
pushStr state l = pure (push (Str c) (next ((Prelude.length c) + 1) state))
    where
        c = C.unpack $ Data.ByteString.takeWhile (/= 0) $ pack l

pushChar :: VMState -> [Word8] -> IO VMState
pushChar state [] = fail "error: PushChar: Missing argument"
pushChar state (x:_) = pure (push (Character (toEnum (fromIntegral x :: Int))) (next 1 state))

pushStruct :: VMState -> [Word8] -> IO VMState
pushStruct _ _ = fail "error: PushStruct: Not implemented"

pushAdress :: VMState -> [Word8] -> IO VMState
pushAdress state [] = fail "error: PushAddress: Missing argument"
pushAdress state (x:y:z:w:_) = pure (push (Adress n) (next 4 state))
    where
        n = (fromIntegral (fromIntegral x :: Int8) :: Int) * 16777216 + (fromIntegral y :: Int) * 65536 + (fromIntegral z :: Int) * 256 + (fromIntegral w :: Int)
pushAdress state _ = fail "error: PushAddress: Invalid argument"

extract :: VMState -> [Word8] -> IO VMState
extract _ _ = fail "error: Extract: Not implemented"

pop :: VMState -> IO VMState
pop state = pure (pop' 1 state)

popN :: VMState -> [Word8] -> IO VMState
popN state [] = fail "error: PopN: Missing argument"
popN state (x:_) = pure (pop' (fromIntegral x :: Int) state)

add :: VMState -> IO VMState
add state = case stack state of
    (a:b:xs) -> push <$> (add' a b) <*> (pure (pop' 2 state))
    _ -> fail "error: Add: Missing argument"

sub :: VMState -> IO VMState
sub state = case stack state of
    -- Flip arguments because of stack
    (a:b:xs) -> push <$> (sub' b a) <*> (pure (pop' 2 state))
    _ -> fail "error: Sub: Missing argument"

mul :: VMState -> IO VMState
mul state = case stack state of
    (a:b:xs) -> push <$> (mul' a b) <*> (pure (pop' 2 state))
    _ -> fail "error: Mul: Missing argument"

div :: VMState -> IO VMState
div state = case stack state of
    -- Flip arguments because of stack
    (a:b:xs) -> push <$> (div' b a) <*> (pure (pop' 2 state))
    _ -> fail "error: Div: Missing argument"

mod :: VMState -> IO VMState
mod state = case stack state of
    -- Flip arguments because of stack
    (a:b:xs) -> push <$> (mod' b a) <*> (pure (pop' 2 state))
    _ -> fail "error: Mod: Missing argument"

eq :: VMState -> IO VMState
eq state = case stack state of
    (a:b:xs) -> push <$> (eq' a b) <*> (pure (pop' 2 state))
    _ -> fail "error: Eq: Missing argument"

neq :: VMState -> IO VMState
neq state = case stack state of
    (a:b:xs) -> push <$> (neq' a b) <*> (pure (pop' 2 state))
    _ -> fail "error: Neq: Missing argument"

lt :: VMState -> IO VMState
lt state = case stack state of
    (a:b:xs) -> push <$> (lt' b a) <*> (pure (pop' 2 state))
    _ -> fail "error: Lt: Missing argument"

gt :: VMState -> IO VMState
gt state = case stack state of
    (a:b:xs) -> push <$> (gt' b a) <*> (pure (pop' 2 state))
    _ -> fail "error: Gt: Missing argument"

leq :: VMState -> IO VMState
leq state = case stack state of
    (a:b:xs) -> push <$> (leq' b a) <*> (pure (pop' 2 state))
    _ -> fail "error: Leq: Missing argument"

geq :: VMState -> IO VMState
geq state = case stack state of
    (a:b:xs) -> push <$> (geq' b a) <*> (pure (pop' 2 state))
    _ -> fail "error: Geq: Missing argument"

or :: VMState -> IO VMState
or state = case stack state of
    (a:b:xs) -> push <$> (or' a b) <*> (pure (pop' 2 state))
    _ -> fail "error: Or: Missing argument"

and :: VMState -> IO VMState
and state = case stack state of
    (a:b:xs) -> push <$> (and' a b) <*> (pure (pop' 2 state))
    _ -> fail "error: And: Missing argument"

not :: VMState -> IO VMState
not state = case stack state of
    (a:xs) -> push <$> (not' a) <*> (pure (pop' 1 state))
    _ -> fail "error: Not: Missing argument"

xor :: VMState -> IO VMState
xor state = case stack state of
    (a:b:xs) -> push <$> (xor' a b) <*> (pure (pop' 2 state))
    _ -> fail "error: Xor: Missing argument"

put :: VMState -> IO VMState
put state = case stack state of
    (a:xs) -> case a of
        Str s -> Prelude.putStr s >> pure (pop' 1 state)
        _ -> fail "error: Put: Invalid argument"
    _ -> fail "error: Put: Missing argument"

display :: VMState -> IO VMState
display state = case stack state of
    (a:xs) -> case a of
        Boolean b -> print b >> pure (pop' 1 state)
        Integer i -> print i >> pure (pop' 1 state)
        Str s -> print s >> pure (pop' 1 state)
        Character c -> print c >> pure (pop' 1 state)
        Struct _ _ _ -> fail "error: Print: Not implemented"
        Adress _ -> fail "error: Print: Not implemented"
    _ -> fail "error: Print: Missing argument"

crash :: VMState -> IO VMState
crash state = case stack state of
    (a:xs) -> case a of
        Boolean b -> fail "error: Crash: Not implemented"
        Integer i -> fail "error: Crash: Not implemented"
        Str s -> fail ("error: " ++ s)
        Character c -> fail "error: Crash: Not implemented"
        Struct _ _ _ -> fail "error: Crash: Not implemented"
        Adress _ -> fail "error: Crash: Not implemented"
    _ -> fail "error: Crash: Missing argument"

show :: VMState -> IO VMState
show state = case stack state of
    (a:xs) -> case a of
        Boolean b -> push <$> (pure $ Str (Prelude.show b)) <*> (pure (pop' 1 state))
        Integer i -> push <$> (pure $ Str (Prelude.show i)) <*> (pure (pop' 1 state))
        Str s -> push <$> (pure $ Str (Prelude.show s)) <*> (pure (pop' 1 state))
        Character c -> push <$> (pure $ Str (Prelude.show c)) <*> (pure (pop' 1 state))
        Struct _ _ _ -> fail "error: Show: Not implemented"
        Adress _ -> fail "error: Show: Not implemented"
    _ -> fail "error: Show: Missing argument"

push :: Typee -> VMState -> VMState
push t state = state { stack = t : stack state }

pop' :: Int -> VMState -> VMState
pop' 0 state = state
pop' n state = state { stack = Prelude.drop n (stack state) }

next :: Int -> VMState -> VMState
next n state = state { rip = rip state + n }

execute' :: VMState -> [Word8] -> IO VMState
execute' state [] = pure state
execute' state l = exec h
    where
        h = Prelude.drop (rip state) l
        exec [] = pure state
        exec l'@(x:xs) = case toEnum (fromIntegral x) of
            Noop -> execute' (next 1 state) l'
            PushBool -> pushBool (next 1 state) xs >>= flip execute' l
            PushInt -> pushInt (next 1 state) xs >>= flip execute' l
            PushStr -> pushStr (next 1 state) xs >>= flip execute' l
            PushChar -> pushChar (next 1 state) xs >>= flip execute' l
            PushStruct -> pushStruct (next 1 state) xs >>= flip execute' l
            PushAdress -> pushAdress (next 1 state) xs >>= flip execute' l
            Extract -> extract (next 1 state) xs >>= flip execute' l
            Pop -> pop (next 1 state) >>= flip execute' l
            PopN -> popN (next 1 state) xs >>= flip execute' l
            Copy -> fail "error: Copy: Not implemented"
            Call -> fail "error: Call: Not implemented"
            Ret -> fail "error: Ret: Not implemented"
            Load -> fail "error: Load: Not implemented"
            End -> fail "error: End: Not implemented"
            Crash -> crash (next 1 state) >>= flip execute' l
            Put -> put (next 1 state) >>= flip execute' l
            Print -> display (next 1 state) >>= flip execute' l
            Add -> add (next 1 state) >>= flip execute' l
            Sub -> sub (next 1 state) >>= flip execute' l
            Mul -> mul (next 1 state) >>= flip execute' l
            Div -> Twillight.div (next 1 state) >>= flip execute' l
            Mod -> Twillight.mod (next 1 state) >>= flip execute' l
            Eq -> eq (next 1 state) >>= flip execute' l
            Neq -> neq (next 1 state) >>= flip execute' l
            Lt -> lt (next 1 state) >>= flip execute' l
            Gt -> gt (next 1 state) >>= flip execute' l
            Leq -> leq (next 1 state) >>= flip execute' l
            Geq -> geq (next 1 state) >>= flip execute' l
            Or -> Twillight.or (next 1 state) >>= flip execute' l
            And -> Twillight.and (next 1 state) >>= flip execute' l
            Not -> Twillight.not (next 1 state) >>= flip execute' l
            Xor -> xor (next 1 state) >>= flip execute' l
            Show -> Twillight.show (next 1 state) >>= flip execute' l

dumpB :: [Word8] -> IO [Word8]
dumpB [] = pure []
dumpB (x:xs) = print x >> (pure xs)
    where
        c = if (fromIntegral x :: Int) > 0 then True else False 

dumpI :: [Word8] -> IO [Word8]
dumpI [] = pure []
dumpI (x:y:z:w:xs) = print n >> (pure xs)
    where
        n = (fromIntegral (fromIntegral x :: Int8) :: Int) * 16777216 + (fromIntegral y :: Int) * 65536 + (fromIntegral z :: Int) * 256 + (fromIntegral w :: Int)

dumpS :: [Word8] -> IO [Word8]
dumpS [] = pure []
dumpS l = print c >> (pure (Prelude.drop ((Prelude.length c) + 1) l))
    where
        c = C.unpack $ Data.ByteString.takeWhile (/= 0) $ pack l

dumpC :: [Word8] -> IO [Word8]
dumpC [] = pure []
dumpC (x:xs) = print c >> (pure xs)
    where
        c = (toEnum (fromIntegral x :: Int) :: Char)

dumpA :: [Word8] -> IO [Word8]
dumpA [] = pure []
dumpA (x:y:z:w:xs) = print n >> (pure xs)
    where
        n = (fromIntegral (fromIntegral x :: Int8) :: Int) * 16777216 + (fromIntegral y :: Int) * 65536 + (fromIntegral z :: Int) * 256 + (fromIntegral w :: Int)

dumpPopN :: [Word8] -> IO [Word8]
dumpPopN [] = pure []
dumpPopN (x:xs) = print n >> (pure xs)
    where
        n = fromIntegral x :: Int

dump' :: [Word8] -> IO ()
dump' [] = pure ()
dump' l@(x:xs) = d
    where
        d = case toEnum (fromIntegral x) of
            Noop -> Prelude.putStrLn "noop" >> dump' xs
            PushBool -> Prelude.putStr "pushbool " >> (dump' =<< dumpB xs)
            PushInt -> Prelude.putStr "pushint " >> (dump' =<< dumpI xs)
            PushStr -> Prelude.putStr "pushstr " >> (dump' =<< dumpS xs)
            PushChar -> Prelude.putStr "pushchar " >> (dump' =<< dumpC xs)
            PushStruct -> Prelude.putStrLn "pushstruct" >> dump' xs
            PushAdress -> Prelude.putStr "pushadress 0x" >> (dump' =<< dumpA xs)
            Extract -> Prelude.putStrLn "extract" >> dump' xs
            Pop -> Prelude.putStrLn "pop" >> dump' xs
            PopN -> Prelude.putStrLn "popn" >> (dump' =<< dumpPopN xs)
            Copy -> Prelude.putStrLn "copy" >> dump' xs
            Call -> Prelude.putStrLn "call" >> dump' xs
            Ret -> Prelude.putStrLn "ret" >> dump' xs
            Load -> Prelude.putStrLn "load" >> dump' xs
            End -> Prelude.putStrLn "end" >> dump' xs
            Crash -> Prelude.putStrLn "crash" >> dump' xs
            Print -> Prelude.putStrLn "print" >> dump' xs
            Add -> Prelude.putStrLn "add" >> dump' xs
            Sub -> Prelude.putStrLn "sub" >> dump' xs
            Mul -> Prelude.putStrLn "mul" >> dump' xs
            Div -> Prelude.putStrLn "div" >> dump' xs
            Mod -> Prelude.putStrLn "mod" >> dump' xs
            Eq -> Prelude.putStrLn "eq" >> dump' xs
            Neq -> Prelude.putStrLn "neq" >> dump' xs
            Lt -> Prelude.putStrLn "lt" >> dump' xs
            Gt -> Prelude.putStrLn "gt" >> dump' xs
            Leq -> Prelude.putStrLn "leq" >> dump' xs
            Geq -> Prelude.putStrLn "geq" >> dump' xs
            Or -> Prelude.putStrLn "or" >> dump' xs
            And -> Prelude.putStrLn "and" >> dump' xs
            Not -> Prelude.putStrLn "not" >> dump' xs
            Xor -> Prelude.putStrLn "xor" >> dump' xs

execute :: [Word8] -> IO VMState
execute l = execute' (VMState { stack = [], rip = 0 }) l

dump :: [Word8] -> IO ()
dump l = dump' l

compilee :: Prog -> IO [Word8]
compilee (Prog s f e) = case lookup3 (Tok 0 0 "main") f of
    Just (_, [], (Simple_ "IO")) -> case lookup (Tok 0 0 "main") e of
        Just (x:xs) ->
            let lex = ((shunting . lexer . snd) x)
                yar = yard =<< lex
            in compile' <$> yar
        Just [] -> fail "error: No entry for main"
        Nothing -> fail "error: No entry for main"
    Just (_, [], _) -> fail "error: main function must return an IO"
    Just (_, _, _) -> fail "error: main function must have no arguments"
    Nothing -> fail "error: No main function"

compileTest :: Prog -> IO VMState
compileTest (Prog s f e) = case lookup3 (Tok 0 0 "main") f of
    Just (_, [], (Simple_ "IO")) -> case lookup (Tok 0 0 "main") e of
        Just (x:xs) ->
            let lex = ((shunting . lexer . snd) x)
                yar = yard =<< lex
                bytecode = compile' <$> yar
            in (print =<< bytecode) >> (execute =<< bytecode)
        Just [] -> fail "error: No entry for main"
        Nothing -> fail "error: No entry for main"
    Just (_, [], _) -> fail "error: main function must return an IO"
    Just (_, _, _) -> fail "error: main function must have no arguments"
    Nothing -> fail "error: No main function"

data Token
    = B Bool
    | Number Int
    | S String
    | C Char
    | AddOp
    | SubOp
    | MulOp
    | DivOp
    | ModOp
    | EqOp
    | NeqOp
    | LtOp
    | GtOp
    | LeqOp
    | GeqOp
    | OrOp
    | AndOp
    | XorOp
    | Open
    | Close
    | Fun String
    deriving (Show, Eq)

data Exp
    = ENumber Int
    | EBool Bool
    | EStr String
    | EChar Char
    | ECall String [Exp]
    deriving (Show, Eq)

instance Compiler Exp where
    compile' (ENumber n) = [fromIntegral (fromEnum PushInt), fromIntegral (fromEnum (n `Prelude.div` 16777216)), fromIntegral (fromEnum (n `Prelude.div` 65536)), fromIntegral (fromEnum (n `Prelude.div` 256)), fromIntegral (fromEnum n)]
    compile' (EBool b) = [fromIntegral (fromEnum PushBool), fromIntegral (fromEnum b)]
    compile' (EStr s) = [fromIntegral (fromEnum PushStr)] ++ (Prelude.map (fromIntegral . fromEnum) s) ++ [0]
    compile' (EChar c) = [fromIntegral (fromEnum PushChar), fromIntegral (fromEnum c)]
    compile' (ECall f a) = (Prelude.concat $ Prelude.map compile' a) ++ (op f)

prec :: Token -> Int
prec (Fun _) = 0
prec AddOp = 6
prec SubOp = 6
prec MulOp = 7
prec ModOp = 7
prec DivOp = 7
prec EqOp = 4
prec NeqOp = 4
prec LtOp = 4
prec GtOp = 4
prec LeqOp = 4
prec GeqOp = 4
prec OrOp = 2
prec AndOp = 3
prec XorOp = 2
prec Open = 10
prec Close = 10

oop :: [(String, Token)]
oop = [
    ("+", AddOp),
    ("-", SubOp),
    ("*", MulOp),
    ("div", DivOp),
    ("mod", ModOp),
    ("eq", EqOp),
    ("neq", NeqOp),
    ("<", LtOp),
    (">", GtOp),
    ("<=", LeqOp),
    (">=", GeqOp),
    ("==", EqOp),
    ("!=", NeqOp),
    ("||", OrOp),
    ("&&", AndOp),
    ("!|", XorOp)
    ]

isOp :: String -> Bool
isOp s = isJust (lookup s oop)

callee :: String -> Maybe Token
callee "print" = Just (Fun "print")
callee "error" = Just (Fun "error")
callee "not" = Just (Fun "not")
callee "put" = Just (Fun "put")
callee "show" = Just (Fun "show")
callee _ = Nothing

eellac :: Token -> String
eellac AddOp = "+"
eellac SubOp = "-"
eellac MulOp = "*"
eellac DivOp = "div"
eellac ModOp = "mod"
eellac EqOp = "eq"
eellac NeqOp = "neq"
eellac LtOp = "<"
eellac GtOp = ">"
eellac LeqOp = "<="
eellac GeqOp = ">="
eellac OrOp = "||"
eellac AndOp = "&&"
eellac XorOp = "!|"
eellac (Fun f) = f

test1 = [Just (Number 2),Just MulOp,Just (Number 2),Just AddOp,Just (Number 2)]
test2 = [Just (Number 1),Just MulOp,Just Open, Just (Number 2),Just SubOp,Just (Number 3), Just Close]

lexer :: D.Expression -> [Maybe Token]
lexer e' = loop (Just e') []
    where
        loop e toks = case e of
            Nothing -> toks
            (Just (D.Literal (fromToken -> D.Integer i))) -> loop Nothing (toks ++ [Just (Number i)])
            (Just (D.Literal (fromToken -> D.Boolean b))) -> loop Nothing (toks ++ [Just (B b)])
            (Just (D.Literal (fromToken -> D.Str s))) -> loop Nothing (toks ++ [Just (S s)])
            (Just (D.Literal (fromToken -> D.Character c))) -> loop Nothing (toks ++ [Just (C c)])
            (Just (D.Call (fromToken -> o) [(fromToken -> x), (fromToken -> y)])) -> loop Nothing (toks ++ (lexer x) ++ [lookup o oop] ++ (lexer y))
            (Just (D.Call (fromToken -> o) a)) -> loop Nothing (toks ++ [callee o] ++ (Prelude.concat $ Prelude.map (lexer . fromToken) a))
            (Just (D.Fold (fromToken -> e))) -> loop Nothing (toks ++ [Just Open] ++ (lexer e) ++ [Just Close])  

shunting :: [Maybe Token] -> IO [Token]
shunting ts = transform ts [] []
    where
        transform [] [] q = pure q
        transform [] s q =
            if (Prelude.head s) == Open
                then fail "error: shunting: Unmatched parenthesis"
                else transform [] (Prelude.tail s) (q ++ [Prelude.head s])
        transform (x:xs) s q = case x of
            (Just (Number n)) -> transform xs s (q ++ [Number n])
            (Just (B b)) -> transform xs s (q ++ [B b])
            (Just (S str)) -> transform xs s (q ++ [S str])
            (Just (C c)) -> transform xs s (q ++ [C c])
            (Just (Fun f)) -> transform xs ((Fun f):s) q
            (Just Open) -> transform xs (Open:s) q
            (Just Close) -> transform xs s' q'
                where
                    s' = Prelude.tail $ Prelude.dropWhile (/= Open) s
                    q' = q ++ (Prelude.takeWhile (/= Open) s)
            (Just o) -> transform xs s'' q'
                where
                    cond x = x /= Open && (prec x >= prec o)
                    s' = Prelude.takeWhile cond s
                    s'' = o : (Prelude.dropWhile cond s)
                    q' = q ++ s'

arity :: Token -> Int
arity (Fun "print") = 1
arity (Fun "error") = 1
arity (Fun "not") = 1
arity (Fun "put") = 1
arity (Fun "show") = 1
arity _ = 2

yard :: [Token] -> IO Exp
yard ts = loop ts []
    where
        loop [] [] = fail "error: yard: Empty expression"
        loop [] [x] = pure x
        loop [] _ = fail "error: yard: Unmatched parenthesis"
        loop l@(x:xs) q = case x of
            (Number n) -> loop xs ((ENumber n) : q)
            (B b) -> loop xs ((EBool b) : q)
            (S s) -> loop xs ((EStr s) : q)
            (C c) -> loop xs ((EChar c) : q)
            o -> loop xs q'
                where
                    q' = if (arity o) > (Prelude.length q)
                        then fail "error: yard: Missing argument"
                        else q''
                    q'' = (ECall (eellac o) (Prelude.reverse q''')) : (Prelude.drop (arity o) q)
                    q''' = Prelude.take (arity o) q

pretty :: Exp -> IO ()
pretty (ENumber n) = Prelude.putStr $ Prelude.show n
pretty (EBool b) = Prelude.putStr $ Prelude.show b
pretty (EStr s) = Prelude.putStr s
pretty (EChar c) = Prelude.putStr $ Prelude.show c
pretty (ECall f a) = case a of
    [x] -> Prelude.putStr f >> pretty x
    [a,b] -> Prelude.putStr "(" >> pretty a >> Prelude.putStr f >> pretty b >> Prelude.putStr ")"
    _ -> fail "error: pretty: Invalid argument"
