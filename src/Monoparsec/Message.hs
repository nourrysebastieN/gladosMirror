
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
merge (Msg t ra re sug) (Msg t' ra' re' sug') = Msg (t <> t') (ra <> ra') (re <> re') (sug <> sug')

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
addSuggestion (Msg t ra re sug) sug' = Msg t ra re (Data.HashSet.insert sug' sug)

displayMessage :: (Show (Token s), Show (Chain s)) =>
    String ->
    String ->
    Message s ->
    IO ()
displayMessage file input (Msg t Empty re sug) = putStr header *> typ *> putStrLn (show re) *> Data.Foldable.foldl _func (pure ()) sug
    where
        _func io sug = io <> (displayMessage file input sug)

        header = file <> ": "
        typ = setSGR [SetColor Foreground Vivid (color t)] *> putStr (show t) *> setSGR [Reset]

displayMessage file input (Msg t ra re sug) = putStr header *> typ *> putStrLn (show re) *> putStr lineInfo *> (line ra) *> printColumnInfo *> Data.Foldable.foldl _func (pure ()) sug
    where
        _func io sug = io <> (displayMessage file input sug)

        _range (Single n) = n
        _range (Range s _) = s
        _range (RangeSingle _ _ n) = n
        _range (All n) = n
        
        _data = takeN (_range ra) input
        _begin = (Monoparsec.Misc.lines . fst) <$> _data
        _end = (Monoparsec.Misc.lines . snd) <$> _data
        __begin = fromMaybe "" (snd <$> (unsnoc =<< _begin))
        _line = fromMaybe "" (snd <$> (unsnoc =<< _begin)) <> fromMaybe "" (fst <$> (uncons =<< _end))
        
        _lineNumber = (show . length) _begin
        _columnNumber = length __begin

        header = file <> ":" <> _lineNumber <> ":" <> (show _columnNumber) <> ": "
        typ = setSGR [SetColor Foreground Vivid (color t)] *> putStr (show t) *> setSGR [Reset]
        lineInfo = "  " <> _lineNumber <> " | "
        
        line (Single _) = (putStr (Data.List.take _columnNumber _line)) *> setSGR [SetColor Foreground Vivid (color t)] *> putStr (Data.List.take 1 (Data.List.drop _columnNumber _line)) *> setSGR [Reset] *> putStrLn (Data.List.drop (_columnNumber + 1) _line)
        line (Range s e) = (putStr (Data.List.take _columnNumber _line)) *> setSGR [SetColor Foreground Vivid (color t)] *> putStr (Data.List.take (e - s + 1) (Data.List.drop _columnNumber _line)) *> setSGR [Reset] *> putStrLn (Data.List.drop (_columnNumber + (e - s + 1)) _line)
        line (All n) = setSGR [SetColor Foreground Vivid (color t)] *> putStrLn _line *> setSGR [Reset]
        line (RangeSingle s e n) = (putStr (Data.List.take (_columnNumber - (n - s)) _line)) *> setSGR [SetColor Foreground Vivid (color t)] *> putStr (Data.List.take (e - s) (Data.List.drop (_columnNumber - (n - s)) _line)) *> setSGR [Reset] *> putStrLn (Data.List.drop ((_columnNumber - (n - s)) + (e - s)) _line)
        
        columnInfoHeader = "  " <> replicate (length _lineNumber) ' ' <> " | "
        
        columnInfo (Single n) = replicate (_columnNumber) ' ' <> "^"
        columnInfo (Range s e) = replicate (_columnNumber) ' ' <> replicate (e - s + 1) '~'
        columnInfo (RangeSingle s e n) = drop (n - s) (replicate (_columnNumber) ' ') <> replicate (n - s) '~' <> "^" <> replicate (e - n) '~'
        columnInfo (All n) = ""
        
        printColumnInfo = putStr columnInfoHeader *> setSGR [SetColor Foreground Vivid (color t)] *> putStrLn (columnInfo ra) *> setSGR [Reset]
