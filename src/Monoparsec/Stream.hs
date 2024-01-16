
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Monoparsec.Stream(
    Stream(..),
    VisualStream(..)
) where

import Data.Kind (Type)
import Data.List
import Data.ByteString
import Data.Word
import Data.Proxy

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

    singleton :: Proxy s -> Token s -> Chain s

class (Stream s) => VisualStream s where
    show' :: Proxy s -> s -> String

instance Stream ByteString where
    type Token ByteString = Word8
    type Chain ByteString = ByteString

    take = Data.ByteString.uncons
    takeN n t
        | n <= 0 = Just (Data.ByteString.empty, t)
        | Data.ByteString.null t = Nothing
        | otherwise = Just (Data.ByteString.splitAt n t)
    takeWhile = Data.ByteString.span
    
    chainLength _ = Data.ByteString.length
    chainToList _ = Data.ByteString.unpack
    listToChain _ = Data.ByteString.pack
    chainEmpty _ = Data.ByteString.null

    singleton _ = Data.ByteString.singleton

instance Stream String where
    type Token String = Char
    type Chain String = String

    take = Data.List.uncons
    takeN n t
        | n <= 0 = Just ([], t)
        | Data.List.null t = Nothing
        | otherwise = Just (Data.List.splitAt n t)
    takeWhile = Data.List.span

    chainLength _ = Data.List.length
    chainToList _ = id
    listToChain _ = id
    chainEmpty _ = Data.List.null

    singleton _ x = [x]

instance VisualStream String where
    show' _ s = if Data.List.null s then "<empty>" else show s
