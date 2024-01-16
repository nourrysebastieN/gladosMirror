{-# LANGUAGE DeriveDataTypeable #-}

module System.Console.Hawk.Implicit.Extra where

import Data.Data

data Extra
    = Help String
    | Name String
    | Explicit
    | Ignore

    | FlagOptional String
    | FlagArgs
    | FlagArgPos Int
    | FlagType String

    | ModeDefault
    | ModeHelpSuffix [String]

    | ProgSummary String
    | ProgProgram String
    | ProgVerbosity
    | ProgHelpArg [Extra]
    | ProgVersionArg [Extra]
    | ProgVerbosityArgs [Extra] [Extra]
    | ProgNoAtExpand
      deriving (Eq,Ord,Show,Data,Typeable)
