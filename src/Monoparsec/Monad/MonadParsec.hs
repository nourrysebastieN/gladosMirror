
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Monoparsec.Monad.MonadParsec (
    MonadParsec(..),
    MonadState(..)
) where

import Control.Monad
import Data.Set

import Monoparsec.Stream
import Monoparsec.Message
import Monoparsec.State
import Monoparsec.Monad.Option

class (MonadPlus m, Stream s) => MonadParsec s m | m -> s where
    token :: (Token s -> Maybe a) -> Set (Item s) -> m a
    chain :: (Chain s -> Chain s -> Bool) -> Chain s -> m (Chain s)

    eof :: m ()
    isEof :: m Bool

    error :: String -> m a
    error' :: Message s -> m a
    absolute :: m a -> m a
    message :: Message s -> m a -> m a
    suggest :: [Message s] -> m a -> m a
    label :: String -> m a -> m a
    hide :: m a -> m a
    hide = label ""
    range :: Range -> m a -> m a

    secure :: m a -> m (Either (Message s) a)
    fallback :: (Int -> Message s -> m a) -> m a -> m a
    overload :: (Int -> Message s -> Message s) -> m a -> m a
    lookup :: (a -> m b) -> m a -> m b
    ahead :: m a -> m a
    maybe :: m a -> m (Maybe a)

    while :: (Token s -> Bool) -> m (Chain s)
    while1 :: (Token s -> Bool) -> m (Chain s)

    trace :: String -> m a -> m a

class (MonadPlus m, Stream s, Option o) => MonadState s o m | m -> s, m -> o where
    getState :: m (State s o)
    getOffset :: m Int
    getInput :: m s
