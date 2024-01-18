
module Monoparsec.Message.Reason (
    Reason(..)
) where

import Control.Applicative
import Data.Set
import Data.Maybe
import Data.Hashable

data Reason i =
        Expectation (Maybe i) (Set i)
    |
        Message String

instance (Ord i) => Semigroup (Reason i) where
    (<>) = merge

instance (Ord i) => Monoid (Reason i) where
    mempty = Message ""

instance (Eq i) => Eq (Reason i) where
    (==) (Expectation u e) (Expectation u' e') = fromMaybe False (liftA2 (==) u u') && e == e'
    (==) (Message m) (Message m') = m == m'
    (==) _ _ = False

instance (Ord i, Hashable i) => Hashable (Reason i) where
    hashWithSalt salt (Expectation u e) = hashWithSalt salt (u, e)
    hashWithSalt salt (Message m) = hashWithSalt salt m

instance (Show i) => Show (Reason i) where
    show (Expectation Nothing e) = "expected " <> (stringSet $ toList e)
    show (Expectation (Just u) e) =
        "unexpected " <> show u <> ", "
        <> "expected " <> (stringSet $ toList e)
    show (Message m) = m

stringSet :: (Show i) => [i] -> String
stringSet [] = ""
stringSet [i] = show i
stringSet (x:[i]) = show x <> " or " <> show i
stringSet (x:xs) = show x <> ", " <> stringSet xs

merge :: (Ord i) => Reason i -> Reason i -> Reason i
merge (Expectation u e) (Expectation u' e') =
    Expectation (liftA2 max u u') (union e e')
merge (Message s) (Message s') = Message (s <> ", " <> s')
merge m@(Message _) _ = m
merge _ m@(Message _) = m
