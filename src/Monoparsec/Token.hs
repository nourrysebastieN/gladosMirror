
module Monoparsec.Token (
    Tok(..),
    fromToken,
    toRange
) where

import Control.Applicative

import Monoparsec.Message

data Tok a = Tok Int Int a deriving (Show)

instance (Eq a) => Eq (Tok a) where
    (==) (Tok _ _ a) (Tok _ _ b) = a == b

instance (Ord a) => Ord (Tok a) where
    compare (Tok _ _ a) (Tok _ _ b) = compare a b

instance Functor Tok where
    fmap f (Tok l c a) = Tok l c (f a)

instance Applicative Tok where
    pure = Tok 0 0
    (Tok _ _ f) <*> (Tok l c a) = Tok l c (f a)

instance Monad Tok where
    return = pure
    (Tok _ _ a) >>= f = f a

instance (Semigroup a, Monoid a) => Monoid (Tok a) where
    mempty = Tok 0 0 mempty

instance Semigroup a => Semigroup (Tok a) where
    (Tok s e a) <> (Tok s' e' b) = Tok (min s s') (max e e') (a <> b)

fromToken :: Tok a -> a
fromToken (Tok _ _ a) = a

toRange :: Tok a -> Range
toRange (Tok s e _) = Range s e
