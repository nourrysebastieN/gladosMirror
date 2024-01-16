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
        sources :! def ! argPos 0 ! typ "sources"
        
        , dump :! def ! explicit ! name "dump" ! help "Dump the code into assembly like format"
    ]
    ! program "dawn"
    ! summary "dawn v0.4.0, (C) Agakistune, nourrysebastienN"

checkOption :: VMOption -> IO VMOption
checkOption opt = when (null $ sources opt) (putErrStrLn "dawn: no input files" >> exitFailure) >> return opt

computeOption :: VMOption -> IO ()
computeOption (Option src d) = exec
    where
        bs = doesFileExist src
        check = file src =<< bs
        file name b = unless b (putErrStrLn ("dawn: " ++ name ++ ": No such file or directory") >> exitFailure) >> return name
        contents = B.readFile =<< check
        exec = exec' =<< contents
        exec' i = (guard d >> (T.dump $ B.unpack i)) <|> (guar =<< (try $ T.execute $ B.unpack i))
        
        guar :: Either IOError T.VMState -> IO ()
        guar (Left e) = putErrStrLn ("dawn: " ++ (ioeGetErrorString e)) >> exitFailure
        guar (Right s) = return ()

main :: IO ()
main = computeOption =<< checkOption =<< (hawk vmOption :: IO VMOption)
