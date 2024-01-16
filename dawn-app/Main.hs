{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import System.Console.Hawk

newtype Sample = Sample {foo :: Int} deriving (Show, Data, Typeable)

-- dummy :: Sample
dummy = build (Sample 0) [
        foo :! def ! help "aaaaa"
    ] ! program "test"

main :: IO ()
main = print =<< (hawk dummy :: IO Sample)
