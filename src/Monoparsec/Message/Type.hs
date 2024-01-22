{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

module Monoparsec.Message.Type (
    Type(..),
    color
) where

import Data.Bifunctor
import Data.Hashable
import System.Console.ANSI

data Type =
        Info
    |
        Warning
    |
        Error

instance Semigroup Type where
    (<>) = merge

merge :: Type -> Type -> Type
merge ta tb = (toEnum . uncurry max . bimap fromEnum fromEnum) (ta,tb)

instance Enum Type where
    toEnum 0 = Info
    toEnum 1 = Warning
    toEnum 2 = Error
    toEnum _ = error "toEnum: invalid argument"
    fromEnum Info = 0
    fromEnum Warning = 1
    fromEnum Error = 2

color :: Type -> Color
color Info = Cyan
color Warning = Yellow
color Error = Red

instance Eq Type where
    (==) a b = fromEnum a == fromEnum b

instance Ord Type where
    compare a b = compare (fromEnum a) (fromEnum b)

instance Bounded Type where
    minBound = Info
    maxBound = Error

instance Monoid Type where
    mempty = Info

instance Hashable Type where
    hashWithSalt salt n = hashWithSalt salt (fromEnum n)

instance Show Type where
    show Warning = "warning: "
    show Error = "error: "
    show Info = "info: "
