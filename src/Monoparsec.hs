
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Monoparsec (
    ParsecT(..),
    Parsec,
    
    testParse,
    runParse,
    runParse',
    runParseT,
    
    satisfy,
    like,
    unlike,
    Monoparsec.any,
    oneOf,
    noneOf,
    Monoparsec.string,
    manyUntil,
    someUntil,
    integer,
    char,
    whitespace,
    tokenize,

    module Monoparsec.State,
    module Msg,
    module Monoparsec.Stream,
    module Monoparsec.Monad,
    module Monoparsec.Token
)
where

import Control.Monad
import Control.Applicative
import qualified Data.List
import Data.Set
import Data.Hashable
import Data.HashSet
import Data.Proxy
import Data.Foldable
import Data.Char
import Data.Functor.Identity

import Monoparsec.State
import qualified Monoparsec.Message as Msg
import Monoparsec.Message.Reason
import Monoparsec.Message.Range
import Monoparsec.Message.Item
import Monoparsec.Stream
import Monoparsec.Monad
import Monoparsec.Token

newtype ParsecT s o m a = ParsecT {
    parse ::
        forall b.
        State s o ->
        (a -> State s o -> m b) ->
        (Msg.Message s -> State s o -> m b) ->
        (Msg.Message s -> State s o -> m b) ->
        m b
}

instance (Stream s, Semigroup a) => Semigroup (ParsecT s o m a) where
    (<>) = liftA2 (<>)

instance (Stream s, Monoid a) => Monoid (ParsecT s o m a) where
    mempty = pure mempty
    mappend = (<>)

instance (Stream s) => Functor (ParsecT s o m) where
    fmap f p = ParsecT $ \s ok err arr -> parse p s (ok . f) err arr

instance (Stream s) => Applicative (ParsecT s o m) where
    pure x = ParsecT $ \s ok _ _ -> ok x s
    (<*>) = Monoparsec.ap
    p1 *> p2 = bind p1 $ const p2
    p1 <* p2 = bind p1 $ \x -> fmap (const x) p2

ap :: ParsecT s o m (a -> b) -> ParsecT s o m a -> ParsecT s o m b
ap left right = ParsecT $ \s ok err arr ->
    let ok' f s' = parse right s' (ok . f) err arr
    in parse left s ok' err arr

bind :: ParsecT s o m a -> (a -> ParsecT s o m b) -> ParsecT s o m b
bind p f = ParsecT $ \s ok err arr ->
    let ok' x s' = parse (f x) s' ok err arr
    in parse p s ok' err arr

instance (Stream s, Hashable (Token s), Hashable (Chain s)) => Alternative (ParsecT s o m) where
    empty = ParsecT $ \s@(State _ _ off _) _ e _ -> e (Msg.error' (Single off) mempty Data.HashSet.empty) s
    (<|>) = plus

instance (Stream s, Monad m) => Monad (ParsecT s o m) where
    return = pure
    (>>=) = bind

instance (Stream s, Monad m) => MonadFail (ParsecT s o m) where
    fail msg = ParsecT $ \s@(State _ _ off _) _ _ a -> a (Msg.error' (Single off) (Msg.Message msg) Data.HashSet.empty) s

instance (Stream s, Hashable (Token s), Hashable (Chain s), Monad m, Option o) => MonadWarn (ParsecT s o m) where
    warn msg = ParsecT $ \s@(State _ o off _) ok _ arr ->
        case (warnOpt o msg) of
            (Left msg) -> arr (Msg.error' (Single off) (Msg.Message msg) Data.HashSet.empty) s
            (Right msg) -> ok () (addMessage (Msg.warning (Single off) (Msg.Message msg)) s)
    warnRange msg ra = ParsecT $ \s@(State _ o off _) ok _ arr ->
        case (warnOpt o msg) of
            (Left msg) -> arr (Msg.error' ra (Msg.Message msg) Data.HashSet.empty) s
            (Right msg) -> ok () (addMessage (Msg.warning ra (Msg.Message msg)) s)

instance (Stream s, Hashable (Token s), Hashable (Chain s), Monad m, Option o) => MonadState s o (ParsecT s o m) where
    getState = ParsecT $ \s ok _ _ -> ok s s
    getOffset = offset <$> getState
    getInput = input <$> getState

instance (Stream s, Hashable (Token s), Hashable (Chain s), Monad m) => MonadPlus (ParsecT s o m) where
    mzero = Control.Applicative.empty
    mplus = plus

plus :: (Stream s, Hashable (Token s), Hashable (Chain s)) => ParsecT s o m a -> ParsecT s o m a -> ParsecT s o m a
plus left right = ParsecT $ \s ok err arr ->
    let err' e' s' =
            let err'' e'' s'' = err (e'' <> e') (s' <> s'')
            in parse right s ok err'' arr
    in parse left s ok err' arr

instance (Stream s, Hashable (Token s), Hashable (Chain s), Monad m, Option o) => MonadParsec s (ParsecT s o m) where
    token test expected = ParsecT $ \s@(State i opt o m) ok err _ ->
        case Monoparsec.Stream.take i of
            Nothing -> err (Msg.error' (Single o) (Expectation (Just EndOfStream) expected) Data.HashSet.empty) s
            Just (c,cs) ->
                case test c of
                    Nothing -> err (Msg.error' (Single o) (Expectation (Just $ Token c) expected) Data.HashSet.empty) s
                    Just x -> ok x (State cs opt (o + 1) m)

    chain test expected = ParsecT $ \s@(State i opt o m) ok err _ ->
        let pxy = Proxy :: Proxy s
            unexpect pos u = Msg.error' (Single pos) (Expectation (Just u) (Data.Set.singleton (Chain expected))) Data.HashSet.empty
            len = chainLength pxy expected
        in case Monoparsec.Stream.takeN len i of
            Nothing -> err (unexpect o EndOfStream) s
            Just (c, cs) ->
                if test expected c
                    then
                        let s' = State cs opt (o + len) m
                        in ok expected s'
                    else err (unexpect o (Chain c)) s

    eof = ParsecT $ \s@(State i _ o m) ok err _ ->
        case Monoparsec.Stream.take i of
            Nothing -> ok () s
            Just (c, _) -> err (Msg.error' (Single o) (Expectation (Just $ EndOfStream) (Data.Set.singleton (Token c))) Data.HashSet.empty) s

    isEof = ParsecT $ \s@(State i _ _ _) ok _ _ ->
        case Monoparsec.Stream.take i of
            Nothing -> ok True s
            Just _ -> ok False s

    error s = getOffset >>= (\o -> Monoparsec.Monad.error' (Msg.error' (Single o) (Msg.Message s) Data.HashSet.empty))

    error' e = ParsecT $ \s _ err _ -> err e s

    absolute p = ParsecT $ \s ok err arr ->
        let err' m s' = arr m s'
        in parse p s ok err' arr

    message m p = ParsecT $ \s ok err arr ->
        let ok' a s' = ok a (addMessage m s')
        in parse p s ok' err arr

    suggest ms p = ParsecT $ \s ok err arr ->
        let ok' a s' = ok a $ Data.Foldable.foldl (\s'' m -> suggest' m s'') s' ms
            err' e s' = err (Data.Foldable.foldl (\e' m -> Msg.addSuggestion e' m) e ms) s'
            arr' e s' = arr (Data.Foldable.foldl (\e' m -> Msg.addSuggestion e' m) e ms) s'
        in parse p s ok' err' arr'

    label l p = ParsecT $ \s ok err arr ->
        let err' e' s'@(State _ _ o _) = err (e' <> Msg.error' (Single o) (Expectation Nothing (Data.Set.singleton (Label l))) Data.HashSet.empty) s'
        in parse p s ok err' arr

    range r p = ParsecT $ \s ok err arr ->
        let err' e s' = err (Msg.setRange e r) s'
            arr' e s' = arr (Msg.setRange e r) s'
        in parse p s ok err' arr'

    secure p = ParsecT $ \s ok _ arr ->
        let err' e s' = ok (Left e) s'
        in parse p s (ok . Right) err' arr

    fallback f p = ParsecT $ \s ok err arr ->
        let err' e (State _ _ o _) = parse (f o e) s ok err arr
        in parse p s ok err' arr

    overload f p = ParsecT $ \s ok err arr ->
        let err' e (State _ _ o _) = err (f o e) s
            arr' e (State _ _ o _) = arr (f o e) s
        in parse p s ok err' arr'

    lookup f p = ParsecT $ \s ok err arr -> 
        let ok' a s' = parse (f a) s' ok err arr
        in parse p s ok' err arr

    ahead p = ParsecT $ \s ok err arr ->
        let ok' a _ = ok a s
        in parse p s ok' err arr

    maybe p = ParsecT $ \s ok _ arr ->
        let ok' a s' = ok (Just a) s'
            err' e s' = ok Nothing s'
        in parse p s ok' err' arr

    while p = ParsecT $ \(State i opt o m) ok err _ ->
        let pxy = Proxy :: Proxy s
            (t, ts) = Monoparsec.Stream.takeWhile p i
            len = chainLength pxy t
        in ok t (State ts opt (o + len) m)

    while1 p = ParsecT $ \s@(State i opt o m) ok err _ ->
        let pxy = Proxy :: Proxy s
            (t, ts) = Monoparsec.Stream.takeWhile p i
            len = chainLength pxy t
        in if chainEmpty pxy t
            then
                case Monoparsec.Stream.take i of
                    Nothing -> err (Msg.error' (Single o) (Expectation (Just EndOfStream) Data.Set.empty) Data.HashSet.empty) s
                    Just (c, _) -> err (Msg.error' (Single o) (Expectation (Just $ Token c) Data.Set.empty) Data.HashSet.empty) s
            else ok t (State ts opt (o + len) m)

satisfy :: (MonadParsec s m) => (Token s -> Bool) -> m (Token s)
satisfy p = token (\c -> if p c then Just c else Nothing) Data.Set.empty

like :: (MonadParsec s m) => Token s -> m (Token s)
like c = token (\x -> if x == c then Just x else Nothing) $ Data.Set.singleton (Token c)

unlike :: (MonadParsec s m) => Token s -> m (Token s)
unlike c = token (\x -> if x /= c then Just x else Nothing) $ Data.Set.empty

any :: (MonadParsec s m) => m (Token s)
any = token Just Data.Set.empty

oneOf :: (MonadParsec s m) => [Token s] -> m (Token s)
oneOf lst = token test $ Data.Set.fromList (Prelude.map Token lst)
    where
        test c =
            if Data.Foldable.elem c lst
                then pure c
                else Control.Applicative.empty

noneOf :: (MonadParsec s m) => [Token s] -> m (Token s)
noneOf lst = satisfy (\c -> Data.Foldable.notElem c lst)

string :: (MonadParsec s m) => Chain s -> m (Chain s)
string = chain (==)

manyUntil :: (MonadParsec s m, Alternative m) => m a -> m end -> m [a]
manyUntil p end = (end *> (pure [])) <|> ((:) <$> p <*> (manyUntil p end))

someUntil :: (MonadParsec s m, Alternative m) => m a -> m end -> m [a]
someUntil p end = (:) <$> p <*> manyUntil p end

integer :: (MonadParsec s m, Alternative m, Token s ~ Char, Num a) => m a
integer = label "integer" $ ((\n -> -n) <$> (like '-' *> _integer)) <|> (like '+' *> _integer) <|> _integer

_integer :: forall s m a. (MonadParsec s m, Alternative m, Token s ~ Char, Num a) => m a
_integer = num <$> (alternate1 <|> alternate2)
    where
        alternate1 = Data.List.singleton <$> like '0' <* t
        alternate2 = (:) <$> (satisfy digit :: m (Token s)) <*> cha <* t
        t = void (ahead (satisfy (not . Data.Char.isAlphaNum))) <|> eof
        cha = chainToList (Proxy :: Proxy s) <$> (while digit0 :: m (Chain s)) :: m [Token s]

        digit0 :: Token s -> Bool
        digit0 c = Data.Char.isDigit c

        digit :: Token s -> Bool
        digit c = Data.Char.isDigit c && c /= '0'

        num = Data.Foldable.foldl' step 0
        step a c = a * 10 + fromIntegral (Data.Char.digitToInt c)

char :: forall s m. (MonadParsec s m, MonadFail m, Token s ~ Char) => m Char
char = consume =<< ahead Monoparsec.any :: m (Token s)
    where
        consume c =
            case c of
                '\\' -> control =<< replicateM_ 1 Monoparsec.any *> ahead Monoparsec.any
                '\a' -> Monoparsec.Monad.error "invalid character"
                '\b' -> Monoparsec.Monad.error "invalid character"
                '\f' -> Monoparsec.Monad.error "invalid character"
                '\n' -> Monoparsec.Monad.error "invalid character"
                '\r' -> Monoparsec.Monad.error "invalid character"
                '\t' -> Monoparsec.Monad.error "invalid character"
                '\v' -> Monoparsec.Monad.error "invalid character"
                '\"' -> Monoparsec.Monad.error "invalid character"
                '\'' -> Monoparsec.Monad.error "invalid character"
                c' -> replicateM_ 1 Monoparsec.any *> return c
        control c =
            case c of
                'a' -> replicateM_ 1 Monoparsec.any *> return '\a'
                'b' -> replicateM_ 1 Monoparsec.any *> return '\b'
                'f' -> replicateM_ 1 Monoparsec.any *> return '\f'
                'n' -> replicateM_ 1 Monoparsec.any *> return '\n'
                'r' -> replicateM_ 1 Monoparsec.any *> return '\r'
                't' -> replicateM_ 1 Monoparsec.any *> return '\t'
                'v' -> replicateM_ 1 Monoparsec.any *> return '\v'
                '\\' -> replicateM_ 1 Monoparsec.any *> return '\\'
                '\"' -> replicateM_ 1 Monoparsec.any *> return '\"'
                '\'' -> replicateM_ 1 Monoparsec.any *> return '\''
                _    -> Monoparsec.Monad.error "invalid control character"

whitespace :: (MonadParsec s m, Token s ~ Char) => m ()
whitespace = label "whitespace" (while Data.Char.isSpace) *> pure ()

tokenize :: (MonadParsec s m, MonadState s o m) => m a -> m (Tok a)
tokenize p = func <$> getOffset <*> p <*> getOffset
    where
        func o1 v o2 = Tok o1 o2 v 

type Parsec s o = ParsecT s o Identity

testParse :: (Option o, s ~ String, Show a) => Parsec s o a -> s -> IO ()
testParse p i = do
    let res = runParse p i
    case res of
        (s'@(State _ _ _ _), Left l) -> Data.Foldable.foldl (<>) (pure ()) (Prelude.map (Msg.displayMessage "test.dawn" i) l)
        (s'@(State _ _ _ l), Right a) -> Data.Foldable.foldl (<>) (pure ()) (Prelude.map (Msg.displayMessage "test.dawn" i) l) *> (putStrLn $ show a)

runParse :: (Option o) => Parsec s o a -> s -> (State s o, Either [Msg.Message s] a)
runParse p s = runParse' p (emptyState s)

runParse' :: Parsec s o a -> State s o -> (State s o, Either [Msg.Message s] a)
runParse' p = runIdentity . runParseT p

runParseT :: (Monad m) => ParsecT s o m a -> State s o -> m (State s o, Either [Msg.Message s] a)
runParseT p s = parse p s ok err err
    where
        ok a s' = pure $ (s', Right a)
        err e s'@(State _ _ _ m) = pure $ (s', Left (e:m))
