
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Monoparsec.State (
    State(..),
    emptyState,
    addMessage,
    suggest'
) where

import Data.Hashable

import Monoparsec.Message
import Monoparsec.Stream
import Monoparsec.Monad.Option

data State s o = State {
    input :: s,
    option :: o,
    offset :: Int,
    messages :: [Message s]
}

instance Semigroup (State s o) where
    (<>) = merge

deriving instance (Eq s, Ord (Token s), Ord (Chain s), Eq o) => Eq (State s o)

merge :: State s o -> State s o -> State s o
merge (State i opt o m) (State i' _ o' m') =
    if o > o'
        then State i opt o (m <> m')
        else State i' opt o' (m <> m')

emptyState :: (Option o) => s -> State s o
emptyState s = State s emptyOpt 0 []

addMessage :: Message s -> State s o -> State s o
addMessage m (State i opt o ms) = State i opt o (m:ms)

suggest' :: (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => Message s -> State s o -> State s o
suggest' m s@(State i opt o []) = addMessage m s
suggest' m s@(State i opt o (m':ms)) = State i opt o $ (addSuggestion m' m) : ms
