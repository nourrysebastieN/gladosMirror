
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Monoparsec.Debug where

import Data.Hashable
import Data.Proxy
import Debug.Trace as D

import Monoparsec

class (MonadParsec s m) => MonadParsercDebug s m where

    debug :: (Show a) => String -> m a -> m a

instance (VisualStream s, Hashable (Token s), Hashable (Chain s), Monad m, Option o) => MonadParsercDebug s (ParsecT s o m) where
    debug lbl p = ParsecT $ \s@(State i _ _ _) ok err arr ->
        let ok' a s' = D.trace (lbl ++ " ok - " ++ (show' (Proxy :: Proxy s) i) ++ " -> " ++ (show a)) $ ok a s'
            err' = D.trace (lbl ++ " err - " ++ (show' (Proxy :: Proxy s) i)) . err
            arr' = D.trace (lbl ++ " arr - " ++ (show' (Proxy :: Proxy s) i)) . arr
        in parse p s ok' err' arr'
