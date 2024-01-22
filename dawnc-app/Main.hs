{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import System.Environment
import System.Exit
import System.IO.Error
import System.IO
import System.Directory
import Control.Exception
import Control.Monad
import Control.Applicative
import Data.Data
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.Word

import Option
import Dawn as P
import Dusk
import Twillight as T

import Monoparsec
import Monoparsec.Message
import System.Console.Hawk

-- data Flag = Verbose deriving(Show)

-- test :: Maybe Int -> Int
-- test (Just a) = a

-- tost :: Int -> Int
-- tost a = test Just a

compilerOption :: Annotate Extra
compilerOption = build (Option {}) [sources :! def ! args ! typ "file"
        , ast :! def ! explicit ! name "ast" ! help "Dump the AST"
        , assembly :! def ! explicit ! name "dump"
        ! help "Dump the code into assembly like format"
        , werror :! def ! explicit ! name "Werror"
        ! help "Treat warnings as errors"
        , output :! def ! explicit ! name "o" ! help "Output file"
        , test :! def ! explicit ! name "test" ! help "Test the code"
    ] ! program "dawnc"
    ! summary "dawnc v0.4.0, (C) Agakistune, nourrysebastienN, Nathan-hoareau"

putErrStr :: String -> IO ()
putErrStr = hPutStr stderr

putErrStrLn :: String -> IO ()
putErrStrLn = hPutStrLn stderr

checkOption :: CompilerOption -> IO CompilerOption
checkOption opt =
    when (null $ sources opt)
        (putErrStrLn "dawnc: no input files" >> exitFailure)
    >> return opt

computeOption :: CompilerOption -> IO ()
computeOption opt@(Option src _ _ _ _ _) = parses >> return ()
    where
        check = zipWithM file src =<< (mapM doesFileExist src)
        file name b = unless b
                (putErrStrLn
                    ("dawnc: " ++ name ++ ": No such file or directory")
                >> exitFailure)
            >> return name
        contents = mapM readFile =<< check
        parses = zipWithM (parse' opt) src =<< contents
        
parse' :: CompilerOption -> String -> String -> IO ()
parse' o name i =
    let res = runParse P.prog i
    in case res of
            (s'@(State _ _ _ _), Left l) ->
                foldl (<>) (pure ()) (map (displayMessage name i) l)
            (s'@(State _ _ _ l), Right a) -> compile'' o name i a

compile'' :: CompilerOption -> String -> String  -> [Tok Declaration] -> IO ()
compile'' opt name i a = case compile a of
    Left l ->
        foldl (<>) (pure ())
            (map (displayMessage name i) l)
    Right a' ->
        when (ast opt) (print a)
        >> ((guard (test opt) >> (void $ compileTest a'))
        <|> ((gate opt) =<< (try $ compilee a')))

gate :: CompilerOption -> Either IOError [Word8] -> IO ()
gate opt (Left e) = putErrStrLn ("dawnc: " ++ (ioeGetErrorString e))
    >> exitFailure
gate opt (Right s) = (guard (assembly opt) >> (dump s))
    <|> (B.writeFile (fromMaybe "a.out" (output opt)) $ B.pack s)

main :: IO ()
main = computeOption
    =<< checkOption
    =<< (hawk compilerOption :: IO CompilerOption)
