module Main (main) where

import System.IO
import System.Exit
import Control.Monad
import Control.Exception
import Lisp

test :: String -> IO ()
test str = pure ()

execute :: Bool -> IO ()
execute False = lispFileInterpreter
execute True = lispCLI

main :: IO ()
main = handler =<< exec
    where
        exec = ((try . execute) =<< hIsTerminalDevice stdin) :: IO (Either SomeException ())
        handler (Left err) = print err *> exitWith (ExitFailure 84)
        handler _ = pure ()