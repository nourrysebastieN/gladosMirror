
{-# LANGUAGE DeriveDataTypeable #-}

module Option where

import Data.Data

import Monoparsec

data CompilerOption = Option {
        sources :: [String],

        ast :: Bool,
        assembly :: Bool,

        werror :: Bool,
        output :: Maybe String
    } deriving (Show, Data, Typeable)

instance Option CompilerOption where
    emptyOpt = Option {
        sources = mempty,
        
        ast = False,
        assembly = False,

        werror = False,
        output = Nothing
    }
    warnOpt opt msg = if werror opt then Left msg else Right msg
