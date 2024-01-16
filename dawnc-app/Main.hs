
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
        , assembly :! def ! explicit ! name "dump" ! help "Dump the code into assembly like format"

        , werror :! def ! explicit ! name "Werror" ! help "Treat warnings as errors"
        , output :! def ! explicit ! name "o" ! help "Output file"
        , test :! def ! explicit ! name "test" ! help "Test the code"
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
computeOption (Option src ast d _ n t) = parses >> return ()
    where
        bs = mapM doesFileExist src
        check = zipWithM file src =<< bs
        file name b = unless b (putErrStrLn ("dawnc: " ++ name ++ ": No such file or directory") >> exitFailure) >> return name
        contents = mapM readFile =<< check
        parses = zipWithM parse src =<< contents
        parse name i = 
            let res = runParse P.prog i
            in case res of
                    (s'@(State _ _ _ _), Left l) -> foldl (<>) (pure ()) (map (displayMessage name i) l)
                    (s'@(State _ _ _ l), Right a) -> case compile a of
                        Left l -> foldl (<>) (pure ()) (map (displayMessage name i) l)
                        -- Right a' -> (guar =<< (try $ when ast (print a) >> (compilee a')))
                        Right a' -> when ast (print a) >> ((guard t >> (void $ compileTest a')) <|> (guar =<< (try $ compilee a')))
        guar :: Either IOError [Word8] -> IO ()
        guar (Left e) = putErrStrLn ("dawnc: " ++ (ioeGetErrorString e)) >> exitFailure
        guar (Right s) = (guard d >> (dump s)) <|> (B.writeFile (fromMaybe "a.out" n) $ B.pack s)

main :: IO ()
main = computeOption =<< checkOption =<< (hawk compilerOption :: IO CompilerOption)
