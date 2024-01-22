{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

module Monoparsec.Message.Range(
    Range(..)
) where

import Data.Hashable

data Range =
        RangeSingle Int Int Int
    |
        Range Int Int
    |
        Single Int
    |
        Empty
    |
        All Int

instance Show Range where
    show (RangeSingle s e a) = show s <> ":" <> show e <> ":" <> show a <> ":"
    show (Range s e) = show s <> ":" <> show e <> ":"
    show (Single n) = show n <> ":"
    show (Empty) = "<empty>"
    show (All n) = show n <> ":"

instance Semigroup Range where
    (<>) = merge

instance Eq Range where
    (==) (RangeSingle s e a) (RangeSingle s' e' a') = s == s' && e == e' && a == a'
    (==) (Range s e) (Range s' e') = s == s' && e == e'
    (==) (Single n) (Single n') = n == n'
    (==) (Empty) (Empty) = True
    (==) (All n) (All n') = n == n'

instance Monoid Range where
    mempty = Empty

instance Hashable Range where
    hashWithSalt salt (RangeSingle s e a) = hashWithSalt salt (s, e, a)
    hashWithSalt salt (Range s e) = hashWithSalt salt (s, e)
    hashWithSalt salt (Single n) = hashWithSalt salt n
    hashWithSalt salt (Empty) = hashWithSalt salt (-1 :: Int)
    hashWithSalt salt (All n) = hashWithSalt salt n

merge :: Range -> Range -> Range
merge (RangeSingle s e a) (RangeSingle s' e' a') =
    RangeSingle (min s s') (max e e') (min a a')
merge (Range s e) (Range s' e') = Range (min s s') (max e e')
merge (Single n) (Single n') =
    if n > n'
        then Single n
        else Range n n'
merge (Empty) (Empty) = Empty
merge (All n) (All n') = All (min n n')

merge _ a@(All n) = a
merge a@(All n) _ = a

merge a (Empty) = a
merge (Empty) a = a

merge (RangeSingle s e a) (Range s' e') = RangeSingle (min s s') (max e e') a
merge (RangeSingle s e a) (Single n) = RangeSingle s e (min a n)

merge (Range s e) (RangeSingle s' e' a) = RangeSingle (min s s') (max e e') a
merge (Range s e) (Single n) = RangeSingle s e n

merge (Single n) (RangeSingle s e a) = RangeSingle s e (min a n)
merge (Single n) (Range s e) = RangeSingle s e n
