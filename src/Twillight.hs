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
    | Print

    | Add
    | Sub
    | Mul
    | Div
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
op "print" = [fromIntegral (fromEnum Print)]
-- op "printB" = [fromIntegral (fromEnum Print)]
-- op "printI" = [fromIntegral (fromEnum Print)]
-- op "printS" = [fromIntegral (fromEnum Print)]
-- op "printC" = [fromIntegral (fromEnum Print)]
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
    fromEnum Print = 0x10
    fromEnum Add = 0x11
    fromEnum Sub = 0x12
    fromEnum Mul = 0x13
    fromEnum Div = 0x14

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
    toEnum 0x10 = Print
    toEnum 0x11 = Add
    toEnum 0x12 = Sub
    toEnum 0x13 = Mul
    toEnum 0x14 = Div

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
div' (Integer a) (Integer b) = when (b == 0) (fail "error: Div: div by 0") >> (pure (Integer (a `Prelude.div` b)))
div' _ _ = fail "error: Div: Invalid argument"

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

display :: VMState -> IO VMState
display state = case stack state of
    (a:xs) -> case a of
        Boolean b -> Prelude.putStrLn (show b) >> pure (pop' 1 state)
        Integer i -> Prelude.putStrLn (show i) >> pure (pop' 1 state)
        Str s -> Prelude.putStrLn s >> pure (pop' 1 state)
        Character c -> Prelude.putStrLn (show c) >> pure (pop' 1 state)
        Struct _ _ _ -> fail "error: Print: Not implemented"
        Adress _ -> fail "error: Print: Not implemented"
    _ -> fail "error: Print: Missing argument"

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
            Crash -> fail "error: Crash: Not implemented"
            Print -> display (next 1 state) >>= flip execute' l
            Add -> add (next 1 state) >>= flip execute' l
            Sub -> sub (next 1 state) >>= flip execute' l
            Mul -> mul (next 1 state) >>= flip execute' l
            Div -> Twillight.div (next 1 state) >>= flip execute' l

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
dumpS l = print c >> (pure (Prelude.drop (Prelude.length c) l))
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

execute :: [Word8] -> IO VMState
execute l = execute' (VMState { stack = [], rip = 0 }) l

dump :: [Word8] -> IO ()
dump l = dump' l

compilee :: Prog -> IO [Word8]
compilee (Prog s f e) = case lookup3 (Tok 0 0 "main") f of
    Just (_, [], (Simple_ "IO")) -> case lookup (Tok 0 0 "main") e of
        Just (x:xs) -> (pure . compile' . snd) x
        Just [] -> fail "error: No entry for main"
        Nothing -> fail "error: No entry for main"
    Just (_, [], _) -> fail "error: main function must return an IO"
    Just (_, _, _) -> fail "error: main function must have no arguments"
    Nothing -> fail "error: No main function"

compileTest :: Prog -> IO VMState
compileTest (Prog s f e) = case lookup3 (Tok 0 0 "main") f of
    Just (_, [], (Simple_ "IO")) -> case lookup (Tok 0 0 "main") e of
        Just (x:xs) -> 
            let bytecode = (compile' . snd) x
            in print bytecode >> (execute bytecode)
        Just [] -> fail "error: No entry for main"
        Nothing -> fail "error: No entry for main"
    Just (_, [], _) -> fail "error: main function must return an IO"
    Just (_, _, _) -> fail "error: main function must have no arguments"
    Nothing -> fail "error: No main function"
