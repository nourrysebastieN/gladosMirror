{-# LANGUAGE RecordWildCards #-}
module System.Console.Hawk.Explicit.ExpandArgsAt(expandArgsAt) where

import System.FilePath

expandArgsAt :: [String] -> IO [String]
expandArgsAt args = do
        ebefore <- mapM (f [] ".") before
        return $ concat ebefore ++ after
    where
        (before,after) = break (== "--") args

        f seen dir ('@':x)
            | x `elem` seen = error $ unlines $
                "System.Console.Hawk.Explicit.expandArgsAt, recursion in @ directives:" :
                map ("  "++) (reverse $ x:seen)
            | length seen > 15 = error $ unlines $
                "System.Console.Hawk.Explicit.expandArgsAt, over 15 @ directives deep:" :
                map ("  "++) (reverse seen)
            | otherwise = do
                src <- readFile $ dir </> x
                fmap concat $ mapM (f (x:seen) (takeDirectory x)) $ lines src
        f _ _ x = return [x]
