{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

{-# LANGUAGE DeriveDataTypeable #-}

module System.Console.Hawk.Verbosity(
    Verbosity(..)
) where

import Control.Monad
import Data.Data

data Verbosity
    = Quiet
    | Normal
    | Loud
      deriving (Eq,Ord,Bounded,Enum,Show,Read,Data,Typeable)
