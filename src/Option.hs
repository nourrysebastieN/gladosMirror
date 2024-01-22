{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Option where

import Data.Data

import Monoparsec

data CompilerOption = Option {
        sources :: [String],

        ast :: Bool,
        assembly :: Bool,

        werror :: Bool,
        output :: Maybe String,
        test :: Bool
    } deriving (Show, Data, Typeable)

instance Option CompilerOption where
    emptyOpt = Option {
        sources = mempty,
        
        ast = False,
        assembly = False,

        werror = False,
        output = Nothing,
        test = False
    }
    warnOpt opt msg = if werror opt then Left msg else Right msg
