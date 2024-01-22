{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Monoparsec.Message (
    Message,
    
    Monoparsec.Message.info,
    Monoparsec.Message.info',
    Monoparsec.Message.infoString,
    Monoparsec.Message.warning,
    Monoparsec.Message.warning',
    Monoparsec.Message.warningString,
    Monoparsec.Message.error,
    Monoparsec.Message.error',
    Monoparsec.Message.errorString,
    Monoparsec.Message.message,
    
    setRange,
    setReason,
    setSuggestion,
    
    getRange,
    getReason,
    getSuggestion,

    addSuggestion,
    
    displayMessage,

    module Monoparsec.Message.Range,
    module Monoparsec.Message.Reason,
    module Monoparsec.Message.Item,
    module Monoparsec.Message.Type
) where

import Data.Hashable
import Data.HashSet
import Data.Maybe
import Data.List
import Data.Foldable
import System.Console.ANSI

import Monoparsec.Message.Range
import Monoparsec.Message.Reason
import Monoparsec.Message.Item
import Monoparsec.Message.Type
import Monoparsec.Stream
import Monoparsec.Misc

data Message s = Msg Type Range (Reason (Item s)) (HashSet (Message s))

instance (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => Semigroup (Message s) where
    (<>) = merge

merge :: (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => Message s -> Message s -> Message s
merge (Msg t ra re sug) (Msg t' ra' re' sug') =
    Msg (t <> t') (ra <> ra') (re <> re') (sug <> sug')

instance (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => Hashable (Message s) where
    hashWithSalt salt (Msg t ra re sug) = hashWithSalt salt (t, ra, re, sug)

instance (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => Monoid (Message s) where
    mempty = Msg mempty mempty mempty mempty

instance (Ord (Token s), Ord (Chain s)) => Eq (Message s) where
    (==) (Msg t ra re sug) (Msg t' ra' re' sug') = t == t' && ra == ra' && re == re' && sug == sug'

instance (Show (Token s), Show (Chain s)) => Show (Message s) where
    show (Msg t ra re sug) = show ra <> show t <> show re <> " - " <> show sug

info :: (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => Range -> (Reason (Item s)) -> (Message s)
info ra re = Msg Info ra re mempty

info' :: Range -> (Reason (Item s)) -> (HashSet (Message s)) -> (Message s)
info' = Msg Info

infoString :: (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => Range -> String -> (Message s)
infoString ra re = Msg Info ra (Message re) mempty

warning :: (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => Range -> (Reason (Item s)) -> (Message s)
warning ra re = Msg Warning ra re mempty

warning' :: Range -> (Reason (Item s)) -> (HashSet (Message s)) -> (Message s)
warning' = Msg Warning

warningString :: (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => Range -> String -> (Message s)
warningString ra re = Msg Warning ra (Message re) mempty

error :: Range -> (Reason (Item s)) -> (Message s)
error ra re = Msg Error ra re empty

error' :: Range -> (Reason (Item s)) -> (HashSet (Message s)) -> (Message s)
error' = Msg Error 

errorString :: (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => Range -> String -> (Message s)
errorString ra re = Msg Error ra (Message re) mempty

message :: (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => String -> (Message s)
message msg = infoString (Empty) msg

setRange :: Message s -> Range -> Message s
setRange (Msg t _ re sug) ra = Msg t ra re sug

setReason :: Message s -> Reason (Item s) -> Message s
setReason (Msg t ra _ sug) re = Msg t ra re sug

setSuggestion :: Message s -> HashSet (Message s) -> Message s
setSuggestion (Msg t ra re _) sug = Msg t ra re sug

getRange :: Message s -> Range
getRange (Msg _ ra _ _) = ra

getReason :: Message s -> Reason (Item s)
getReason (Msg _ _ re _) = re

getSuggestion :: Message s -> HashSet (Message s)
getSuggestion (Msg _ _ _ sug) = sug

addSuggestion :: (Ord (Token s), Ord (Chain s), Hashable (Token s), Hashable (Chain s)) => Message s -> Message s -> Message s
addSuggestion (Msg t ra re sug) sug' =
    Msg t ra re (Data.HashSet.insert sug' sug)

putColorStr :: Color -> String -> IO ()
putColorStr c s =
    setSGR [SetColor Foreground Vivid c]
    *> putStr s
    *> setSGR [Reset]

_range :: Range -> Int
_range (Single n) = n
_range (Range s _) = s
_range (RangeSingle _ _ n) = n
_range (All n) = n

lineData :: Range -> String -> (String,Int,Int)
lineData ra input = (_line, (length . fromJust) _begin, length __begin)
    where
        _data = takeN (_range ra) input
        _begin = (Monoparsec.Misc.lines . fst) <$> _data
        _end = (Monoparsec.Misc.lines . snd) <$> _data
        __begin = fromMaybe "" (snd <$> (unsnoc =<< _begin))
        _line =
            fromMaybe "" (snd <$> (unsnoc =<< _begin))
            <> fromMaybe "" (fst <$> (uncons =<< _end))

preLine :: (String,Int,Int) -> String
preLine (_,l,_) = "  " <> (show l) <> " | "

line :: Type -> Range -> (String,Int,Int) -> IO ()

line t (Single _) (_line,_lineNumber,_columnNumber) =
    (putStr (Data.List.take _columnNumber _line))
    *> putColorStr (color t)
        (Data.List.take 1 (Data.List.drop _columnNumber _line))
    *> putStrLn (Data.List.drop (_columnNumber + 1) _line)

line t (Range s e) (_line,_lineNumber,_columnNumber) =
    (putStr (Data.List.take _columnNumber _line))
    *> putColorStr (color t)
        (Data.List.take (e - s + 1)
            (Data.List.drop _columnNumber _line))
    *> putStrLn (Data.List.drop (_columnNumber + (e - s + 1)) _line)

line t (All n) (_line,_lineNumber,_columnNumber) =
    putColorStr (color t) _line

line t (RangeSingle s e n) (_line,_lineNumber,_columnNumber) =
    (putStr (Data.List.take (_columnNumber - (n - s)) _line))
    *> putColorStr (color t)
        (Data.List.take (e - s + 1)
            (Data.List.drop (_columnNumber - (n - s)) _line))
    *> putStrLn
        (Data.List.drop ((_columnNumber - (n - s)) + (e - s)) _line)

file :: String -> (String,Int,Int) -> String
file f (_,l,c) =  f <> ":" <> (show l) <> ":" <> (show c) <> ": "

columnHead :: Int -> String
columnHead n = "  " <> replicate ((length . show) n) ' ' <> " | "

_column :: Range -> (String,Int,Int) -> String
_column (Single _) (_,_,c) = replicate c ' ' <> "^"
_column (Range s e) (_,_,c) =
    replicate c ' ' <> replicate (e - s + 1) '~'
_column (RangeSingle s e n) (_,_,c) =
    drop (n - s) (replicate c ' ')
    <> replicate (n - s) '~' <> "^" <> replicate (e - n) '~'
_column (All _) _ = ""

column :: Type -> Range -> (String,Int,Int) -> IO ()
column t ra tu@(_,l,c) =
    putStr (columnHead l)
    *> putColorStr (color t) (_column ra tu)
    *> putStrLn ""

displayMessage :: (Show (Token s), Show (Chain s)) =>
    String ->
    String ->
    Message s ->
    IO ()
displayMessage file input (Msg t Empty re sug) =
    putStr header
    *> typ
    *> putStrLn (show re)
    *> Data.Foldable.foldl _func (pure ()) sug
    where
        _func io sug = io <> (displayMessage file input sug)

        header = file <> ": "
        typ = putColorStr (color t) (show t)

displayMessage f input (Msg t ra re sug) =
    putStr (file f dat)
    *> putColorStr (color t) (show t)
    *> putStrLn (show re)
    *> putStr (preLine dat) *> (line t ra dat)
    *> (column t ra dat)
    *> Data.Foldable.foldl _func (pure ()) sug
    where
        _func io sug = io <> (displayMessage f input sug)
        dat = lineData ra input
