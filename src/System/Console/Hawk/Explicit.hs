{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

{-# LANGUAGE ScopedTypeVariables, CPP #-}

module System.Console.Hawk.Explicit(

    process, processArgs, processValue, processValueIO,

    module System.Console.Hawk.Explicit.Type,
    flagHelpSimple, flagHelpFormat, flagVersion, flagNumericVersion, flagsVerbosity,

    module System.Console.Hawk.Explicit.Help,

    module System.Console.Hawk.Explicit.ExpandArgsAt,
    module System.Console.Hawk.Explicit.SplitJoin,
    Complete(..), complete
    ) where

import System.Console.Hawk.Explicit.Type
import System.Console.Hawk.Explicit.Process
import System.Console.Hawk.Explicit.Help
import System.Console.Hawk.Explicit.ExpandArgsAt
import System.Console.Hawk.Explicit.SplitJoin
import System.Console.Hawk.Explicit.Complete
import System.Console.Hawk.Default
import System.Console.Hawk.Helper
import System.Console.Hawk.Text
import System.Console.Hawk.Verbosity

import Control.Monad
import Data.Char
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

processArgs :: Mode a -> IO a
processArgs m = do
    env <- getEnvironment
    case lookup "CMDARGS_COMPLETE" env of
        Just x -> do
            args <- getArgs
            let argInd = fromMaybe (length args - 1) $ readMay x
                argPos =
                    fromMaybe
                        (
                            if argInd >= 0 && argInd < length args
                            then length (args !! argInd)
                            else 0
                        ) $
                         readMay =<< lookup "CMDARGS_COMPLETE_POS" env
            print $ complete m (concatMap words args) (argInd,argPos)
            exitWith ExitSuccess
        Nothing -> do
            nam <- getProgName
            let var = mplus
                            (
                                lookup (
                                    "CMDARGS_HELPER_"
                                    ++ show
                                        (map toUpper $ head $ modeNames m
                                        ++ [nam])
                                )
                            env)
                            (lookup "CMDARGS_HELPER" env)
            case var of
                Nothing -> processValueIO m
                    =<< (if modeExpandAt m then expandArgsAt else return)
                    =<< getArgs
                Just cmd -> do
                    res <- execute cmd m []
                    case res of
                        Left err ->
                            (hPutStrLn stderr $ "Error when running helper "
                            ++ cmd)
                            >> (hPutStrLn stderr err)
                            >> exitFailure
                        Right args -> processValueIO m args

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                [x] -> Just x
                _ -> Nothing

processValue :: Mode a -> [String] -> a
processValue m xs = case process m xs of
    Left x -> errorWithoutStackTrace x
    Right x -> x

processValueIO :: Mode a -> [String] -> IO a
processValueIO m xs = case process m xs of
    Left x -> hPutStrLn stderr x >> exitFailure
    Right x -> return x

flagHelpSimple :: (a -> a) -> Flag a
flagHelpSimple f = flagNone ["help","h"] f "Display help message"

flagHelpFormat :: (HelpFormat -> TextFormat -> a -> a) -> Flag a
flagHelpFormat f =
    (flagOpt "" ["help","h"] upd "" "Display help message"){
        flagInfo = FlagOptRare ""}
    where
        upd s v = case format s of
            Left e -> Left e
            Right (a,b) -> Right $ f a b v

        format :: String -> Either String (HelpFormat,TextFormat)
        format xs = foldl (\acc x -> f x =<< acc) (Right def) (sep xs)
            where
                sep = words . map (\x ->
                    if x `elem` ":," then ' ' else toLower x)
                f x (a,b) = case x of
                    "all" -> Right (HelpFormatAll,b)
                    "one" -> Right (HelpFormatOne,b)
                    "def" -> Right (HelpFormatDefault,b)
                    "html" -> Right (a,HTML)
                    "text" -> Right (a,defaultWrap)
                    _ | all isDigit x -> Right (a,Wrap $ read x)
                    _ -> Left m

m :: String
m = "unrecognised help format, expected one of: all one def html text <NUMBER>"

flagVersion :: (a -> a) -> Flag a
flagVersion f = flagNone ["version","V"] f "Print version information"

flagNumericVersion :: (a -> a) -> Flag a
flagNumericVersion f =
    flagNone ["numeric-version"] f "Print just the version number"

flagsVerbosity :: (Verbosity -> a -> a) -> [Flag a]
flagsVerbosity f =
    [flagNone ["verbose","v"] (f Loud) "Loud verbosity"
    ,flagNone ["quiet","q"] (f Quiet) "Quiet verbosity"]
