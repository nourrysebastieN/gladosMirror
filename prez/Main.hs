{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import System.Console.Hawk

data Option =
    Option {
        url :: String,
        method :: String,
        body :: Maybe String
    }
    deriving (Show, Data, Typeable)

option :: Annotate Extra
option = build (Option {})
    [
        url :! def ! argPos 0 ! typ "URL"
        , method :! "GET" ! explicit ! name "method"
            ! help "HTTP method" ! typ "method"
        , body :! def ! explicit ! name "body" ! help "HTTP body" ! typ "body"
    ] ! program "my_curl"
    ! summary "my_curl v1.0.0 (C) Agakistune"

main :: IO ()
main = (hawk option :: IO Option) >>= print
