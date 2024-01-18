{-# LANGUAGE ScopedTypeVariables, CPP #-}
{-|
    This module constructs command lines. You may either use the helper functions
    ('flagNone', 'flagOpt', 'mode' etc.) or construct the type directly. These
    types are intended to give all the necessary power to the person constructing
    a command line parser.

    For people constructing simpler command line parsers, the module
    "System.Console.Hawk.Implicit" may be more appropriate.

    As an example of a parser:

    @
    arguments :: 'Mode' [(String,String)]
    arguments = 'mode' \"explicit\" [] \"Explicit sample program\" ('flagArg' (upd \"file\") \"FILE\")
        ['flagOpt' \"world\" [\"hello\",\"h\"] (upd \"world\") \"WHO\" \"World argument\"
        ,'flagReq' [\"greeting\",\"g\"] (upd \"greeting\") \"MSG\" \"Greeting to give\"
        ,'flagHelpSimple' ((\"help\",\"\"):)]
        where upd msg x v = Right $ (msg,x):v
    @

    And this can be invoked by:

    @
    main = do
        xs <- 'processArgs' arguments
        if (\"help\",\"\") \`elem\` xs then
            print $ 'helpText' [] 'HelpFormatDefault' arguments
         else
            print xs
    @

    /Groups/: The 'Group' structure allows flags/modes to be grouped for the purpose of
    displaying help. When processing command lines, the group structure is ignored.

    /Modes/: The Explicit module allows multiple mode programs by placing additional modes
    in 'modeGroupModes'. Every mode is allowed sub-modes, and thus multiple levels of mode
    may be created. Given a mode @x@ with sub-modes @xs@, if the first argument corresponds
    to the name of a sub-mode, then that sub-mode will be applied. If not, then the arguments
    will be processed by mode @x@. Consequently, if you wish to force the user to explicitly
    enter a mode, simply give sub-modes, and leave 'modeArgs' as @Nothing@. Alternatively, if
    you want one sub-mode to be selected by default, place all it's flags both in the sub-mode
    and the outer mode.

    /Parsing rules/: Command lines are parsed as per most GNU programs. Short arguments single
    letter flags start with @-@, longer flags start with @
    an argument. Anything after @

  > -f

    This command line passes one single letter flag (@f@), one longer flag (@flag@) and two arguments
    (@argument1@ and @
-}
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
                argPos = fromMaybe (if argInd >= 0 && argInd < length args then length (args !! argInd) else 0) $
                         readMay =<< lookup "CMDARGS_COMPLETE_POS" env
            print $ complete m (concatMap words args) (argInd,argPos)
            exitWith ExitSuccess
        Nothing -> do
            nam <- getProgName
            let var = mplus (lookup ("CMDARGS_HELPER_" ++ show (map toUpper $ head $ modeNames m ++ [nam])) env)
                            (lookup "CMDARGS_HELPER" env)
            case var of
                Nothing -> processValueIO m =<< (if modeExpandAt m then expandArgsAt else return) =<< getArgs
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
flagHelpSimple f = flagNone ["help","?"] f "Display help message"

flagHelpFormat :: (HelpFormat -> TextFormat -> a -> a) -> Flag a
flagHelpFormat f =
    (flagOpt "" ["help","?"] upd "" "Display help message"){
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
