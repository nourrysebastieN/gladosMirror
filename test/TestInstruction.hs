module TestInstruction where

import Test.HUnit
import Instructions
import Monoparsec
import Data.Word

exampleVm :: [Word8]
exampleVm =
  [ 0x00, 0x00 -- PushInt 0
  , 0x10, 0x00 -- Copy 0
  , 0x00, 0x0a -- PushInt 10
  , 0x0b -- Less
  , 0x0e, 0x07 -- JumpIf
  , 0x00, 0x01 -- PushInt 1
  , 0x03 -- Add
  , 0x0d, 0xf6 -- Jump -11
  , 0x0c
  ]

exampleAdd :: [Word8]
exampleAdd =
  [ 0x00, 0x0c -- PushInt 12
  , 0x00, 0x02 -- PushInt 2
  , 0x03 -- Add
  , 0x0c
  ]

exampleSub :: [Word8]
exampleSub =
  [ 0x00, 0x0c -- PushInt 12
  , 0x00, 0x02 -- PushInt 14
  , 0x04 -- sub
  , 0x0c
  ]

exampleMul :: [Word8]
exampleMul =
  [ 0x00, 0x04 -- PushInt 12
  , 0x00, 0x04 -- PushInt 2
  , 0x05 -- Mul
  , 0x0c
  ]

exampleDiv :: [Word8]
exampleDiv =
  [ 0x00, 0x06 -- PushInt 12
  , 0x00, 0x06 -- PushInt 2
  , 0x06 -- Div
  , 0x0c
  ]

exampleMod :: [Word8]
exampleMod =
  [ 0x00, 0x01 -- PushInt 12
  , 0x00, 0x07 -- PushInt 2
  , 0x07 -- Mod
  , 0x0c
  ]

printExampleVm :: IO String
printExampleVm = show <$> executeProgram exampleVm

testVm1 :: Test
testVm1 = TestCase $ do
  output <- printExampleVm
  (assertEqual "First Vm" ("VMState {stack = [10], halted = True}") (output))

testVmAdd :: Test
testVmAdd = TestCase $ do
  output <- show <$> executeProgram exampleAdd
  (assertEqual "First Vm" ("VMState {stack = [14], halted = True}") (output))

testVmSub :: Test
testVmSub = TestCase $ do
  output <- show <$> executeProgram exampleSub
  (assertEqual "First Vm" ("VMState {stack = [10], halted = True}") (output))

testVmMul :: Test
testVmMul = TestCase $ do
  output <- show <$> executeProgram exampleMul
  (assertEqual "First Vm" ("VMState {stack = [16], halted = True}") (output))

testVmMod :: Test
testVmMod = TestCase $ do
  output <- show <$> executeProgram exampleMod
  (assertEqual "First Vm" ("VMState {stack = [1], halted = True}") (output))

testVmDiv :: Test
testVmDiv = TestCase $ do
  output <- show <$> executeProgram exampleDiv
  (assertEqual "First Vm" ("VMState {stack = [1], halted = True}") (output))

testCalc = TestList [testVmAdd, testVmDiv, testVmMod, testVmMul, testVmSub]

testInstruction1 = TestCase (assertEqual "Right PushInt" (Instructions.PushInt 2) (decodeInstruction 0x00 0x02))
testInstruction2 = TestCase (assertEqual "Right Eq" (Instructions.Eq) (decodeInstruction 0x01 0))
testInstruction3 = TestCase (assertEqual "Right NEq" (Instructions.NEq) (decodeInstruction 0x02 0))
testInstruction4 = TestCase (assertEqual "Right Add" (Instructions.Add) (decodeInstruction 0x03 0))
testInstruction5 = TestCase (assertEqual "Right Sub" (Instructions.Sub) (decodeInstruction 0x04 0))
testInstruction6 = TestCase (assertEqual "Right Mul" (Instructions.Mul) (decodeInstruction 0x05 0))
testInstruction7 = TestCase (assertEqual "Right Div" (Instructions.Div) (decodeInstruction 0x06 0))
testInstruction8 = TestCase (assertEqual "Right Mod" (Instructions.Mod) (decodeInstruction 0x07 0))
testInstruction9 = TestCase (assertEqual "Right Less" (Instructions.Less) (decodeInstruction 0x08 0))
testInstruction10 = TestCase (assertEqual "Right LessEq" (Instructions.LessEq) (decodeInstruction 0x09 0))
testInstruction11 = TestCase (assertEqual "Right Great" (Instructions.Great) (decodeInstruction 0x0a 0))
testInstruction12 = TestCase (assertEqual "Right GreatEq" (Instructions.GreatEq) (decodeInstruction 0x0b 0))
testInstruction13 = TestCase (assertEqual "Right Halt" (Instructions.Halt) (decodeInstruction 0x0c 0))
testInstruction14 = TestCase (assertEqual "Right Jump" (Instructions.Jump 3) (decodeInstruction 0x0d 0x03))
testInstruction15 = TestCase (assertEqual "Right JumpIf" (Instructions.JumpIf 4) (decodeInstruction 0x0e 0x04))
testInstruction16 = TestCase (assertEqual "Right JumpIfNot" (Instructions.JumpIfNot 5) (decodeInstruction 0x0f 0x05))
testInstruction17 = TestCase (assertEqual "Right Copy" (Instructions.Copy 6) (decodeInstruction 0x10 0x06))
testInstruction18 = TestCase (assertEqual "Right Pop" (Instructions.Pop) (decodeInstruction 0x11 0))
testInstruction19 = TestCase (assertEqual "Right PopN" (Instructions.PopN 2) (decodeInstruction 0x12 0x02))
testInstruction20 = TestCase (assertEqual "Right Debug" (Instructions.Debug) (decodeInstruction 0xff 0))

testInstructions = TestList [
    testInstruction1,
    testInstruction2,
    testInstruction3,
    testInstruction4,
    testInstruction5,
    testInstruction6,
    testInstruction7,
    testInstruction8,
    testInstruction9,
    testInstruction10,
    testInstruction11,
    testInstruction12,
    testInstruction13,
    testInstruction14,
    testInstruction15,
    testInstruction16,
    testInstruction17,
    testInstruction18,
    testInstruction19,
    testInstruction20
  ]

-- testVm1 = TestCase (assertEqual "First Vm" ("[0]\n[1]\n[2]\n[3]\n[4]\n[5]\n[6]\n[7]\n[8]\n[9]\n[10]\nVMState {stack = [10], halted = True}") (printExampleVm))
