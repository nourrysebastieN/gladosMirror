{-|
    This module describes the attributes that can be specified on flags and modes.

    Many attributes have examples specified on the following data type:

    > data Sample = Sample {hello :: String}
-}
module System.Console.Hawk.Implicit.UI where

import System.Console.Hawk.Implicit.Extra
import Data.Typeable

opt :: (Show a, Typeable a) => a -> Extra
opt x = FlagOptional $ case cast x of
    Just y -> y
    _ -> show x

typ :: String -> Extra
typ = FlagType

typFile :: Extra
typFile = typ "FILE"

typDir :: Extra
typDir = typ "DIR"

help :: String -> Extra
help = Help

{-

group :: String -> Extra
group = FldGroup
-}

name :: String -> Extra
name = Name

args :: Extra
args = FlagArgs

argPos :: Int -> Extra
argPos = FlagArgPos

-- groupname :: String -> Extra
-- groupname = GroupName

details :: [String] -> Extra
details = ModeHelpSuffix

summary :: String -> Extra
summary = ProgSummary

auto :: Extra
auto = ModeDefault

program :: String -> Extra
program = ProgProgram

explicit :: Extra
explicit = Explicit

ignore :: Extra
ignore = Ignore

verbosity :: Extra
verbosity = ProgVerbosity

helpArg :: [Extra] -> Extra
helpArg = ProgHelpArg

versionArg :: [Extra] -> Extra
versionArg = ProgVersionArg

verbosityArgs :: [Extra] -> [Extra] -> Extra
verbosityArgs = ProgVerbosityArgs

noAtExpand :: Extra
noAtExpand = ProgNoAtExpand
