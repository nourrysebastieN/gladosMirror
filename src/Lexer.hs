{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-RUN-4-1-compressor--raphael.turpin
-- File description:
-- Lexer
-}

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


module Lexer
(
    Stream(..),
    State(..),
    ErrorItem(..),
    ErrorFancy(..),
    ParseError(..),
    ParsecT(..),
    Parsec,
    MonadParsec(..),
    satisfy,
    like,
    Lexer.any,
    oneOf,
    noneOf,
    string,
    manyUntil,
    someUntil,
    integer,
    whitespace,
    failure,
    complexFailure,
    runParse,
    runParse',
    runParseT
)
where

import Control.Monad
import Control.Applicative

import Data.Kind (Type)
import qualified Data.List
import Data.Functor.Identity
import Data.Text
import Data.Set
import Data.Data
import Data.Foldable
import Data.Char

class (Ord (Token s), Ord (Chain s), Show (Token s), Show (Chain s)) => Stream s where
    type Token s :: Type
    type Chain s :: Type

    take :: s -> Maybe (Token s, s)
    takeN :: Int -> s -> Maybe (Chain s, s)
    takeWhile :: (Token s -> Bool) -> s -> (Chain s, s)
    
    chainLength :: Proxy s -> Chain s -> Int
    chainToList :: Proxy s -> Chain s -> [Token s]
    listToChain :: Proxy s -> [Token s] -> Chain s
    chainEmpty :: Proxy s -> Chain s -> Bool

instance Stream Text where
    type Token Text = Char
    type Chain Text = Text

    take t = Data.Text.uncons t
    takeN n t
        | n <= 0 = Just (Data.Text.empty, t)
        | Data.Text.null t = Nothing
        | otherwise = Just (Data.Text.splitAt n t)
    takeWhile = Data.Text.span
    
    chainLength _ = Data.Text.length
    chainToList _ = Data.Text.unpack
    listToChain _ = Data.Text.pack
    chainEmpty _ = Data.Text.null

instance (Ord a, Show a) => Stream [a] where
    type Token [a] = a
    type Chain [a] = [a]

    take [] = Nothing
    take (t:ts) = Just (t, ts)
    takeN n t
        | n <= 0 = Just ([], t)
        | Data.List.null t = Nothing
        | otherwise = Just (Data.List.splitAt n t)
    takeWhile = Data.List.span

    chainLength _ = Data.List.length
    chainToList _ = id
    listToChain _ = id
    chainEmpty _ = Data.List.null

data State s = State {
    input :: s,
    offset :: Int,
    errors :: [ParseError s]
}

initialState :: s -> State s
initialState s = State s 0 []

data ErrorItem i =
        EndOfStream
    |
        Label String
    |
        Token [i]
    deriving (Eq, Ord)

instance Show i => Show (ErrorItem i) where
    show (EndOfStream) = "end of stream"
    show (Label s) = s
    show (Token s) = show s

data ErrorFancy =
        ErrorFail String
    deriving (Eq, Ord)

instance Show ErrorFancy where
    show (ErrorFail s) = s

data ParseError s =
        SimpleError Int (Maybe (ErrorItem (Token s))) (Set (ErrorItem (Token s)))
--                  ^    ^                             ^
--                offset unexpected                  expected
    |
        ComplexError Int (Set (ErrorFancy))

instance (Show (Token s)) => Show (ParseError s) where
    show (SimpleError i (Just e) expected) | Data.Set.null expected = "Error at " ++ show i ++ ": " ++ show e
                                           | otherwise = "Error at " ++ show i ++ ": " ++ show e ++ ", expected " ++ show expected
    show (SimpleError i Nothing expected) | Data.Set.null expected =  "Error at " ++ show i
                                          | otherwise = "Error at " ++ show i ++ ", expected " ++ show expected
    show (ComplexError i e) = "Error at " ++ show i ++ ": " ++ show e

instance (Stream s) => Semigroup (ParseError s) where
    (<>) = mergeError

instance (Stream s) => Monoid (ParseError s) where
    mempty = SimpleError 0 Nothing Data.Set.empty

getOffset :: ParseError s -> Int
getOffset (SimpleError i _ _) = i
getOffset (ComplexError i _) = i

mergeError :: (Stream s) => ParseError s -> ParseError s -> ParseError s
mergeError e1 e2 = 
    case compare (getOffset e1) (getOffset e2) of
        LT -> e2
        GT -> e1
        EQ -> 
            case (e1, e2) of
                (SimpleError s1 u1 p1, SimpleError _ u2 p2) -> SimpleError s1 (liftA2 max u1 u2) (Data.Set.union p1 p2)
                (ComplexError {}, SimpleError {}) -> e1
                (SimpleError {}, ComplexError {}) -> e2
                (ComplexError s1 f1, ComplexError _ f2) -> ComplexError s1 (Data.Set.union f1 f2)

newtype ParsecT s m a = ParsecT {
    parse ::
        forall b.
        State s ->
        (a -> State s -> m b) -> -- Consume
        (ParseError s -> State s -> m b) -> -- Error
        m b
}

type Parsec s a = ParsecT s Identity a

instance (Stream s) => Functor (ParsecT s m) where
    fmap f p = ParsecT $ \s c e -> parse p s (c . f) e

instance (Stream s) => Applicative (ParsecT s m) where
    pure x = ParsecT $ \s c _ -> c x s
    (<*>) = Lexer.ap
    p1 *> p2 = bind p1 $ const p2
    p1 <* p2 = bind p1 $ \x -> fmap (const x) p2

instance (Stream s) => Alternative (ParsecT s m) where
    empty = ParsecT $ \s@(State _ off _) _ e -> e (SimpleError off Nothing Data.Set.empty) s
    (<|>) = plus

instance (Stream s, Monad m) => Monad (ParsecT s m) where
    return = pure
    (>>=) = bind

instance (Stream s, MonadPlus m) => MonadPlus (ParsecT s m) where
    mzero = Control.Applicative.empty
    mplus = plus

ap :: ParsecT s m (a -> b) -> ParsecT s m a -> ParsecT s m b
ap p1 p2 = ParsecT $ \s c e ->
    let c' f s' = parse p2 s' (c . f) e
    in parse p1 s c' e

bind :: ParsecT s m a -> (a -> ParsecT s m b) -> ParsecT s m b
bind p f = ParsecT $ \s c e -> parse p s (\a s' -> parse (f a) s' c e) e

plus :: (Stream s) => ParsecT s m a -> ParsecT s m a -> ParsecT s m a
plus l r = ParsecT $ \s c err ->
    let err' e' s' =
            let err'' e'' s'' = err (e'' <> e') (longest s' s'')
            in parse r s c err''
    in parse l s c err'

longest :: State s -> State s -> State s
longest l@(State _ off1 _) r@(State _ off2 _) =
    case compare off1 off2 of
        LT -> r
        GT -> l
        EQ -> r

class (Monad m, Stream s) => MonadParsec s m | m -> s where
    token :: (Token s -> Maybe a) -> Set (ErrorItem (Token s)) -> m a
    chain :: (Chain s -> Chain s -> Bool) -> Chain s -> m (Chain s)
    try :: m a -> m a
    eof :: m ()
    secure :: m a -> m (Either (ParseError s) a)
    fallback :: (ParseError s -> m a) -> m a -> m a
    error :: ParseError s -> m a
    label :: String -> m a -> m a
    
    hide :: m a -> m a
    hide = label ""

    while :: (Token s -> Bool) -> m (Chain s)
    while1 :: (Token s -> Bool) -> m (Chain s)

instance (Stream s, Monad m) => MonadParsec s (ParsecT s m) where
    token test set = ParsecT $ \s@(State i o err) consume e ->
        case Lexer.take i of
            Nothing -> e (SimpleError o (Just EndOfStream) Data.Set.empty) s
            Just (c, cs) -> 
                case test c of
                    Nothing -> e (SimpleError o (Just $ Token [c]) set) s
                    Just x -> consume x (State cs (o + 1) err)

    chain = _chain

    try p = ParsecT $ \s c e -> 
        let e' err _ = e err s
        in parse p s c e'

    eof = ParsecT $ \s@(State i o _) consume e ->
        case Lexer.take i of
            Nothing -> consume () s
            Just (c, _) -> e (SimpleError o (Just $ Token [c]) (Data.Set.singleton EndOfStream)) s

    secure p = ParsecT $ \s c _ ->
        let e' err s' = c (Left err) s'
        in parse p s (c . Right) e'

    fallback f p = ParsecT $ \s c e ->
        let e' err s' = parse (f err) s' c e
        in parse p s c e'

    error e = ParsecT $ \s _ err -> err e s

    label l p = ParsecT $ \s c e ->
        let e' err s' = e (ComplexError (getOffset err) (Data.Set.singleton $ ErrorFail l)) s'
        in parse p s c e'

    while = _while
    while1 = _while1

_chain ::
    forall s m.
    (Stream s) =>
    (Chain s -> Chain s -> Bool) ->
    Chain s ->
    ParsecT s m (Chain s)
_chain test c = ParsecT $ \s@(State i o err) consume e ->
    let pxy = Proxy :: Proxy s
        unexpected pos' u =
            let pu = pure u
                ps = (Data.Set.singleton . Token . chainToList pxy) c
            in SimpleError pos' pu ps
        len = chainLength pxy c
    in case Lexer.takeN len i of
        Nothing -> e (SimpleError o (Just EndOfStream) Data.Set.empty) s
        Just (c', cs) ->
            if test c c'
                then consume c (State cs (o + len) err)
                else
                    let ps = (Token . chainToList pxy) c'
                    in e (unexpected o ps) s

_while ::
    forall s m.
    (Stream s) =>
    (Token s -> Bool) ->
    ParsecT s m (Chain s)
_while p = ParsecT $ \(State i o err) consume _ ->
    let pxy = Proxy :: Proxy s
        (t, ts) = Lexer.takeWhile p i
        len = chainLength pxy t
    in consume t (State ts (o + len) err)

_while1 ::
    forall s m.
    (Stream s) =>
    (Token s -> Bool) ->
    ParsecT s m (Chain s)
_while1 p = ParsecT $ \s@(State i o err) consume e ->
    let pxy = Proxy :: Proxy s
        (t, ts) = Lexer.takeWhile p i
        len = chainLength pxy t
    in if chainEmpty pxy t
        then
            let err' = pure $
                    case Lexer.take i of
                        Nothing -> EndOfStream
                        Just (c, _) -> Token [c]
            in e (SimpleError o err' Data.Set.empty) s
        else consume t (State ts (o + len) err)

satisfy :: (MonadParsec s m) => (Token s -> Bool) -> m (Token s)
satisfy p = token (\c -> if p c then Just c else Nothing) Data.Set.empty

like :: (MonadParsec s m) => Token s -> m (Token s)
like c = token (\x -> if x == c then Just x else Nothing) $ Data.Set.singleton (Token [c])

any :: (MonadParsec s m) => m (Token s)
any = token Just Data.Set.empty

oneOf :: (MonadParsec s m) => [Token s] -> m (Token s)
oneOf lst = token (\c -> if Data.Foldable.elem c lst then Just c else Nothing) $ Data.Set.singleton (Token lst)

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

_integer :: forall s m a. (MonadParsec s m, Token s ~ Char, Num a) => m a
_integer = num <$> while1 Data.Char.isDigit
    where
        num = Data.Foldable.foldl' step 0 . chainToList (Proxy :: Proxy s)
        step a c = a * 10 + fromIntegral (Data.Char.digitToInt c)

whitespace :: (MonadParsec s m, Token s ~ Char) => m ()
whitespace = label "whitespace" (while Data.Char.isSpace) *> pure ()

failure :: (MonadParsec s m) => Int -> Maybe (ErrorItem (Token s)) -> [ErrorItem (Token s)] -> m a
failure offset unexpected expected = Lexer.error (SimpleError offset unexpected (Data.Set.fromList expected))

complexFailure :: (MonadParsec s m) => Int -> [ErrorFancy] -> m a
complexFailure offset fancy = Lexer.error (ComplexError offset (Data.Set.fromList fancy))



runParse :: Parsec s a -> s -> Either [ParseError s] a
runParse p s = snd $ runParse' p (initialState s)

runParse' :: Parsec s a -> State s -> (State s, Either [ParseError s] a)
runParse' p = runIdentity . runParseT p

runParseT :: (Monad m) => ParsecT s m a -> State s -> m (State s, Either [ParseError s] a)
runParseT p s = parse p s c e
    where
        c a s' = pure $ (s', Right a)
        e err s' = pure $ (s', Left [err])
