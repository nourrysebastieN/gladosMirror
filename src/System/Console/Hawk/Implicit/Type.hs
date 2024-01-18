{-# LANGUAGE DeriveDataTypeable #-}

module System.Console.Hawk.Implicit.Type(

    Hawk(..), hawkHasValue, embed, reembed,
    CmdArgsPrivate, incArgsSeen, getArgsSeen
    ) where

import System.Console.Hawk.Verbosity

import Data.Data
import Data.Maybe

data Hawk a = Hawk
    {hawkValue :: a
    ,hawkHelp :: Maybe String
    ,hawkVersion :: Maybe String
    ,hawkVerbosity :: Maybe Verbosity
    ,hawkPrivate :: CmdArgsPrivate
    }
    deriving (Show,Eq,Ord,Data,Typeable)

hawkHasValue :: Hawk a -> Bool
hawkHasValue x = isNothing (hawkHelp x) && isNothing (hawkVersion x)

instance Functor Hawk where
    fmap f x = x{hawkValue = f $ hawkValue x}

embed :: a -> Hawk a
embed x = Hawk x Nothing Nothing Nothing (CmdArgsPrivate 0)

reembed :: Hawk a -> (a, a -> Hawk a)
reembed x = (hawkValue x, \y -> x{hawkValue=y})

data CmdArgsPrivate = CmdArgsPrivate
    Int
    deriving (Eq,Ord,Data,Typeable)

incArgsSeen :: Hawk a -> Hawk a
incArgsSeen x@Hawk{hawkPrivate = CmdArgsPrivate i} =
    x{hawkPrivate = CmdArgsPrivate (i+1)}

getArgsSeen :: Hawk a -> Int
getArgsSeen Hawk{hawkPrivate = CmdArgsPrivate i} = i

instance Show CmdArgsPrivate where show _ = "CmdArgsPrivate"
