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
import System.Console.Hawk
import System.Directory
import Control.Exception
import Control.Monad
import Control.Applicative
import qualified Data.ByteString as B

import qualified Twillight as T

data VMOption = Option {
        sources :: String,

        dump :: Bool
    } deriving (Show, Data, Typeable)

putErrStr :: String -> IO ()
putErrStr = hPutStr stderr

putErrStrLn :: String -> IO ()
putErrStrLn = hPutStrLn stderr

vmOption :: Annotate Extra
vmOption = build (Option {})
    [
        sources :! def ! argPos 0 ! typ "binary"
        
        , dump :! def ! explicit ! name "dump"
        ! help "Dump the code into assembly like format"
    ] ! program "dawn"
    ! summary "dawn v0.4.0, (C) Agakistune, nourrysebastienN"

checkOption :: VMOption -> IO VMOption
checkOption opt =
    when (null $ sources opt)
        (putErrStrLn "dawn: no input files" >> exitFailure)
    >> return opt

computeOption :: VMOption -> IO ()
computeOption (Option src d) = exec =<< B.readFile =<< check
    where
        check = file src =<< (doesFileExist src)
        file name b = unless b
                (putErrStrLn ("dawn: "++ name ++ ": No such file or directory")
                >> exitFailure)
            >> return name
        exec i =
            (guard d >> (T.dump $ B.unpack i))
            <|> (gate =<< (try $ T.execute $ B.unpack i))
        
gate :: Either IOError T.VMState -> IO ()
gate (Left e) = putErrStrLn ("dawn: " ++ (ioeGetErrorString e)) >> exitFailure
gate (Right s) = return ()

main :: IO ()
main = computeOption =<< checkOption =<< (hawk vmOption :: IO VMOption)
