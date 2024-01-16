module Instructions where

import Data.Word
import qualified Data.List
import Data.Maybe
import Data.Int
import Control.Monad
import Control.Applicative
import Control.Exception

data Instruction
    = PushInt Int
    | Eq
    | NEq
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Less
    | LessEq
    | Great
    | GreatEq
    | Halt
    | Jump Int
    | JumpIf Int
    | JumpIfNot Int
    | Copy Int
    | Pop
    | PopN Int
    | Debug
    | None
    deriving (Show, Eq)

data VMState = VMState
    { stack :: [Int]
    , halted :: Bool
    } deriving (Show)

decodeInstruction :: Word8 -> Int -> Instruction
decodeInstruction byte n =
    case byte of
        0x00 -> PushInt n
        0x01 -> Eq
        0x02 -> NEq
        0x03 -> Add
        0x04 -> Sub
        0x05 -> Mul
        0x06 -> Div
        0x07 -> Mod
        0x08 -> Less
        0x09 -> LessEq
        0x0a -> Great
        0x0b -> GreatEq
        0x0c -> Halt
        0x0d -> Jump n
        0x0e -> JumpIf n
        0x0f -> JumpIfNot n
        0x10 -> Copy n
        0x11 -> Pop
        0x12 -> PopN n
        0xff -> Debug
        _ -> None

performBinaryOperation :: (Int -> Int -> Int) -> IO VMState -> IO VMState
performBinaryOperation op vmState = func =<< vmState
    where
        func state = case stack state of
            x : y : rest    -> func' state rest =<< (try (evaluate (op y x)) :: IO (Either ArithException Int))
            _               -> fail "Stack underflow"
        func' state rest c  = case c of
            Right val -> pure state { stack = val : rest }
            Left _ -> fail "divide by zero"

copy :: Int -> VMState -> IO VMState
copy n state = (guard (isJust nth) >> pure (state { stack = fromJust nth : stack state })) <|> fail "Index out of range"
    where
        getNth _ [] = empty
        getNth 0 (x:_) = pure x
        getNth i s = getNth (i - 1) (Data.List.tail s)
        nth = getNth n (stack state)

pop :: Int -> VMState -> IO VMState
pop 0 state = (guard (isJust s) >> pure (state { stack = fromJust s })) <|> fail "Stack has too few elements"
    where
        getStack [] = empty
        getStack (_:xs) = pure xs
        s = getStack (stack state)
pop n state
    | n < 0 = fail "Index out of range"
    | otherwise = pure (state { stack = Data.List.drop n (stack state) })

executeInstruction :: IO VMState -> [Word8] -> [Word8] -> IO VMState
executeInstruction vmState [] _ = vmState
executeInstruction vmState [a] _ =
    case decodeInstruction a 0 of
        PushInt _   -> fail "Missing argument"
        Eq          -> performBinaryOperation (\x y -> fromEnum (x == y)) vmState
        NEq         -> performBinaryOperation (\x y -> fromEnum (x /= y)) vmState
        Add         -> performBinaryOperation (+) vmState
        Sub         -> performBinaryOperation (-) vmState
        Mul         -> performBinaryOperation (*) vmState
        Div         -> performBinaryOperation div vmState
        Mod         -> performBinaryOperation mod vmState
        Less        -> performBinaryOperation (\x y -> fromEnum (x < y)) vmState
        LessEq      -> performBinaryOperation (\x y -> fromEnum (x <= y)) vmState
        Great       -> performBinaryOperation (\x y -> fromEnum (x > y)) vmState
        GreatEq     -> performBinaryOperation (\x y -> fromEnum (x >= y)) vmState
        Halt        -> (\s -> s { halted = True }) <$> vmState
        Jump _      -> fail "Missing argument"
        JumpIf _    -> fail "Missing argument"
        JumpIfNot _    -> fail "Missing argument"
        Copy _      -> fail "Missing argument"
        Pop         -> pop 0 =<< vmState
        PopN _      -> fail "Missing argument"
        None        -> fail "Unexpected instruction"
        Debug       -> ((print . stack) =<< vmState) >> vmState
executeInstruction vmState (a:b:xs) initList =
    case decodeInstruction a (fromIntegral (fromIntegral b :: Int8) :: Int) of
        PushInt n   -> executeInstruction ((\s -> s { stack = n : stack s }) <$> vmState) xs initList
        Eq          -> executeInstruction (performBinaryOperation (\x y -> fromEnum (x == y)) vmState) (b : xs) initList
        NEq         -> executeInstruction (performBinaryOperation (\x y -> fromEnum (x /= y)) vmState) (b : xs) initList
        Add         -> executeInstruction (performBinaryOperation (+) vmState) (b : xs) initList
        Sub         -> executeInstruction (performBinaryOperation (-) vmState) (b : xs) initList
        Mul         -> executeInstruction (performBinaryOperation (*) vmState) (b : xs) initList
        Div         -> executeInstruction (performBinaryOperation div vmState) (b : xs) initList
        Mod         -> executeInstruction (performBinaryOperation mod vmState) (b : xs) initList
        Less        -> executeInstruction (performBinaryOperation (\x y -> fromEnum (x < y)) vmState) (b : xs) initList
        LessEq      -> executeInstruction (performBinaryOperation (\x y -> fromEnum (x <= y)) vmState) (b : xs) initList
        Great       -> executeInstruction (performBinaryOperation (\x y -> fromEnum (x > y)) vmState) (b : xs) initList
        GreatEq     -> executeInstruction (performBinaryOperation (\x y -> fromEnum (x >= y)) vmState) (b : xs) initList
        Halt        -> (\s -> s { halted = True }) <$> vmState
        Jump n      -> executeInstruction vmState (getNewList n) initList
        JumpIf n    -> cond n =<< vmState
        JumpIfNot n -> nCond n =<< vmState
        Copy n      -> executeInstruction (copy n =<< vmState) xs initList
        Pop         -> executeInstruction (pop 0 =<< vmState) (b : xs) initList
        PopN n      -> executeInstruction (pop n =<< vmState) xs initList
        None        -> fail "Unexpected instruction"
        Debug       -> ((print . stack) =<< vmState) >> executeInstruction vmState (b:xs) initList
    where
        nCond n state = (guard (func state) >> executeInstruction (pop 0 =<< vmState) xs initList) <|> executeInstruction (pop 0 =<< vmState) (getNewList n) initList
        cond n state = (guard (func state) >> executeInstruction (pop 0 =<< vmState) (getNewList n) initList) <|> executeInstruction (pop 0 =<< vmState) xs initList
        func state = maybe False ((== 1) . fst) (Data.List.uncons (stack state))
        getNewList n = Data.List.drop (fromJust (Data.List.elemIndex a initList) + n) initList

executeProgram :: [Word8] -> IO VMState
executeProgram bytes = executeInstruction initialVMState bytes bytes
    where
        initialVMState = pure VMState { stack = [], halted = False }

exampleProgram :: [Word8]
exampleProgram =
  [ 0x00, 0x00 -- PushInt 0
  , 0xff
  , 0x10, 0x00 -- Copy 0
  , 0x00, 0x0a -- PushInt 10
  , 0x0b -- Less
  , 0x0e, 0x07 -- JumpIf
  , 0x00, 0x01 -- PushInt 1
  , 0x03 -- Add
  , 0x0d, 0xf5 -- Jump -11
  , 0x0c
  ]

testInstruction :: IO()
testInstruction = print =<< executeProgram exampleProgram

printWord8 :: Word8 -> IO()
printWord8 = print