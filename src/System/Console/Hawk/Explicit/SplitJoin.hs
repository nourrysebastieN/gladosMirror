{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

{-# LANGUAGE RecordWildCards #-}
module System.Console.Hawk.Explicit.SplitJoin(splitArgs, joinArgs) where

import Data.Char
import Data.Maybe

joinArgs :: [String] -> String
joinArgs = unwords . map f
    where
        f x = q ++ g x ++ q
            where
                hasSpace = any isSpace x
                q = ['\"' | hasSpace || null x]

                g ('\\':'\"':xs) = '\\':'\\':'\\':'\"': g xs
                g "\\" | hasSpace = "\\\\"
                g ('\"':xs) = '\\':'\"': g xs
                g (x:xs) = x : g xs
                g [] = []

data State = Init
           | Norm
           | Quot

splitArgs :: String -> [String]
splitArgs = join . f Init
    where

        join :: [Maybe Char] -> [String]
        join [] = []
        join xs = map fromJust a : join (drop 1 b)
            where (a,b) = break isNothing xs

        f Init (x:xs) | isSpace x = f Init xs
        f Init "\"\"" = [Nothing]
        f Init "\"" = [Nothing]
        f Init xs = f Norm xs
        f m ('\"':'\"':'\"':xs) = Just '\"' : f m xs
        f m ('\\':'\"':xs) = Just '\"' : f m xs
        f m ('\\':'\\':'\"':xs) = Just '\\' : f m ('\"':xs)
        f Norm ('\"':xs) = f Quot xs
        f Quot ('\"':'\"':xs) = Just '\"' : f Norm xs
        f Quot ('\"':xs) = f Norm xs
        f Norm (x:xs) | isSpace x = Nothing : f Init xs
        f m (x:xs) = Just x : f m xs
        f m [] = []
