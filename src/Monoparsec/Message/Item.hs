
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Monoparsec.Message.Item (
    Item(..)
) where

import Data.Hashable

import Monoparsec.Stream

data Item s =
        EndOfStream
    |
        Label String
    |
        Chain (Chain s)
    |
        Token (Token s)

deriving instance (Ord (Chain s), Ord (Token s)) => Ord (Item s)

instance (Eq (Chain s), Eq (Token s)) => Eq (Item s) where
    (==) (EndOfStream) (EndOfStream) = True
    (==) (Label s) (Label s') = s == s'
    (==) (Chain s) (Chain s') = s == s'
    (==) (Token s) (Token s') = s == s'
    (==) _ _ = False

instance (Hashable (Chain s), Hashable (Token s)) => Hashable (Item s) where
    hashWithSalt salt (EndOfStream) = hashWithSalt salt (0 :: Int)
    hashWithSalt salt (Label s) = hashWithSalt salt s
    hashWithSalt salt (Chain s) = hashWithSalt salt s
    hashWithSalt salt (Token s) = hashWithSalt salt s

instance (Show (Token s), Show (Chain s)) => Show (Item s) where
    show (EndOfStream) = "end of stream"
    show (Label s) = s
    show (Chain s) = show s
    show (Token s) = show s
