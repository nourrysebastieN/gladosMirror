
{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import System.Environment
import System.Exit
import System.IO.Error
import System.IO
import System.Console.Hawk
import System.Directory
import Control.Exception
import Control.Monad
import Data.Data

import Option
import Dawn as P
import Dusk
import Twillight as T
import Monoparsec
import Monoparsec.Message

-- data Flag = Verbose deriving(Show)

-- test :: Maybe Int -> Int
-- test (Just a) = a

-- tost :: Int -> Int
-- tost a = test Just a

compilerOption :: Annotate Extra
compilerOption = build (Option {})
    [
        sources :! def ! args ! typ "sources"
        
        , ast :! def ! explicit ! name "ast" ! help "Dump the AST"
        , assembly :! def ! explicit ! name "asm" ! help "Dump the code into assembly like format"

        , werror :! def ! explicit ! name "Werror" ! help "Treat warnings as errors"
    ]
    ! program "dawnc"
    ! summary "dawnc v0.4.0, (C) Agakistune, nourrysebastienN"

putErrStr :: String -> IO ()
putErrStr = hPutStr stderr

putErrStrLn :: String -> IO ()
putErrStrLn = hPutStrLn stderr

checkOption :: CompilerOption -> IO CompilerOption
checkOption opt = when (null $ sources opt) (putErrStrLn "dawnc: no input files" >> exitFailure) >> return opt

computeOption :: CompilerOption -> IO ()
computeOption opt = parses >> return ()
    where
        bs = mapM doesFileExist (sources opt)
        check = zipWithM file (sources opt) =<< bs
        file name b = unless b (putErrStrLn ("dawnc: " ++ name ++ ": No such file or directory") >> exitFailure) >> return name
        contents = mapM readFile =<< check
        parses = zipWithM parse (sources opt) =<< contents
        parse name i = 
            let res = runParse P.prog i
            in case res of
                    (s'@(State _ _ _ _), Left l) -> foldl (<>) (pure ()) (map (displayMessage name i) l)
                    (s'@(State _ _ _ l), Right a) -> case compile a of
                        Left l -> foldl (<>) (pure ()) (map (displayMessage name i) l)
                        Right a -> displayProg a

main :: IO ()
main = computeOption =<< checkOption =<< (hawk compilerOption :: IO CompilerOption)

test :: IO ()
test = parse "b.dawn" =<< readFile "b.dawn"
    where
        parse name i =
            let res = runParse P.prog i
            in case res of
                (s'@(State _ _ _ _), Left l) -> foldl (<>) (pure ()) (map (displayMessage name i) l)
                (s'@(State _ _ _ l), Right a) -> case compile a of
                    Left l -> foldl (<>) (pure ()) (map (displayMessage name i) l)
                    Right a -> displayProg a

-- a :: Maybe Int Int -> Int
-- a = (+)
