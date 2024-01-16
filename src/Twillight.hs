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

data Type
    = Boolean Bool
    | Integer Int
    | Str String
    | Character Char
    | Struct Int Int [Type]
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
    deriving (Show, Eq)

data VMState = VMState
    { 
        stack :: [Type],
        rip :: Int
    } deriving (Show, Eq)

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

pushBool :: VMState -> [Word8] -> IO VMState
pushBool state [] = fail "Error: PushBool: Missing argument"
pushBool state (x:_) = pure (push (Boolean c) (next 1 state))
    where
        c = if (fromIntegral x :: Int) > 0 then True else False 

pushInt :: VMState -> [Word8] -> IO VMState
pushInt state [] = fail "Error: PushInt: Missing argument"
pushInt state (x:y:z:w:_) = pure (push (Integer n) (next 4 state))
    where
        n = (fromIntegral (fromIntegral x :: Int8) :: Int) * 16777216 + (fromIntegral y :: Int) * 65536 + (fromIntegral z :: Int) * 256 + (fromIntegral w :: Int)
pushInt state _ = fail "Error: PushInt: Invalid argument"

pushStr :: VMState -> [Word8] -> IO VMState
pushStr state [] = fail "Error: PushStr: Missing argument"
pushStr state l = pure (push (Str c) (next ((Prelude.length c) + 1) state))
    where
        c = C.unpack $ Data.ByteString.takeWhile (/= 0) $ pack l

pushChar :: VMState -> [Word8] -> IO VMState
pushChar state [] = fail "Error: PushChar: Missing argument"
pushChar state (x:_) = pure (push (Character (toEnum (fromIntegral x :: Int))) (next 1 state))

pushStruct :: VMState -> [Word8] -> IO VMState
pushStruct _ _ = fail "Error: PushStruct: Not implemented"

pushAdress :: VMState -> [Word8] -> IO VMState
pushAdress state [] = fail "Error: PushAddress: Missing argument"
pushAdress state (x:y:z:w:_) = pure (push (Adress n) (next 4 state))
    where
        n = (fromIntegral (fromIntegral x :: Int8) :: Int) * 16777216 + (fromIntegral y :: Int) * 65536 + (fromIntegral z :: Int) * 256 + (fromIntegral w :: Int)
pushAdress state _ = fail "Error: PushAddress: Invalid argument"

extract :: VMState -> [Word8] -> IO VMState
extract _ _ = fail "Error: Extract: Not implemented"

pop :: VMState -> IO VMState
pop state = pure (pop' 1 state)

popN :: VMState -> [Word8] -> IO VMState
popN state [] = fail "Error: PopN: Missing argument"
popN state (x:_) = pure (pop' (fromIntegral x :: Int) state)

push :: Type -> VMState -> VMState
push t state = state { stack = t : stack state }

pop' :: Int -> VMState -> VMState
pop' 0 state = state
pop' n state = state { stack = Prelude.drop n (stack state) }

next :: Int -> VMState -> VMState
next n state = state { rip = rip state + n }

execute' :: VMState -> [Word8] -> IO VMState
execute' state [] = pure state
execute' state l = check h
    where
        h = Prelude.drop (rip state) l
        check [] = pure state
        check l'@(x:xs) = case toEnum (fromIntegral x) of
            Noop -> execute' (next 1 state) l'
            PushBool -> pushBool (next 1 state) xs >>= flip execute' l'
            PushInt -> pushInt (next 1 state) xs >>= flip execute' l'
            PushStr -> pushStr (next 1 state) xs >>= flip execute' l'
            PushChar -> pushChar (next 1 state) xs >>= flip execute' l'
            PushStruct -> pushStruct (next 1 state) xs >>= flip execute' l'
            PushAdress -> pushAdress (next 1 state) xs >>= flip execute' l'
            Extract -> extract (next 1 state) xs >>= flip execute' l'
            Pop -> pop (next 1 state) >>= flip execute' l'
            PopN -> popN (next 1 state) xs >>= flip execute' l'
            Copy -> fail "Error: Copy: Not implemented"
            Call -> fail "Error: Call: Not implemented"
            Ret -> fail "Error: Ret: Not implemented"
            Load -> fail "Error: Load: Not implemented"
            End -> fail "Error: End: Not implemented"
            Crash -> fail "Error: Crash: Not implemented"
            Print -> fail "Error: Print: Not implemented"

execute :: [Word8] -> IO VMState
execute l = execute' (VMState { stack = [], rip = 0 }) l
