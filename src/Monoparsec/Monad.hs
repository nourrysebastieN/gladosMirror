{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

module Monoparsec.Monad (
    MonadWarn(..),

    module Monoparsec.Monad.MonadParsec,
    module Monoparsec.Monad.Option
) where

import Monoparsec.Message.Range
import Monoparsec.Monad.MonadParsec
import Monoparsec.Monad.Option

class (Monad m) => MonadWarn m where
    warn :: String -> m ()
    warnRange :: String -> Range -> m ()
