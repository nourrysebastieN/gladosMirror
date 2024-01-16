
module Monoparsec.Monad.Option (
    Option(..)
) where

class Option a where
    emptyOpt :: a
    warnOpt :: a -> String -> Either String String
