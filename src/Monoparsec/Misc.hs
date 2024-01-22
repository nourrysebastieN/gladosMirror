{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

module Monoparsec.Misc (
    unsnoc,
    Monoparsec.Misc.lines
) where

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\i -> Just . maybe ([], i) (\(~(a, b)) -> (i : a, b))) Nothing

lines :: String -> [String]
lines "" = [""]
lines s = cons $ broke $ break (== '\n') s
    where
        cons ~(b,e) = b : e
        broke (b, s') = (b, cas s')
        cas [] = []
        cas (_:s'') = Monoparsec.Misc.lines s''
