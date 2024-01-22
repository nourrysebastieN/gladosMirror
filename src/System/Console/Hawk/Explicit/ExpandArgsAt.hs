{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

{-# LANGUAGE RecordWildCards #-}
module System.Console.Hawk.Explicit.ExpandArgsAt(expandArgsAt) where

import System.FilePath

e1 :: String
e1 = "System.Console.Hawk.Explicit.expandArgsAt, recursion in @ directives:"

e2 :: String
e2 = "System.Console.Hawk.Explicit.expandArgsAt, over 15 @ directives deep:"

expandArgsAt :: [String] -> IO [String]
expandArgsAt args = do
        ebefore <- mapM (f [] ".") before
        return $ concat ebefore ++ after
    where
        (before,after) = break (== "--") args

        f seen dir ('@':x)
            | x `elem` seen = error $ unlines $
                e1 :
                map ("  "++) (reverse $ x:seen)
            | length seen > 15 = error $ unlines $
                e2 :
                map ("  "++) (reverse seen)
            | otherwise = do
                src <- readFile $ dir </> x
                fmap concat $ mapM (f (x:seen) (takeDirectory x)) $ lines src
        f _ _ x = return [x]
