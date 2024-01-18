{-# LANGUAGE PatternGuards #-}

module System.Console.Hawk.Explicit.Complete(
    Complete(..), complete,
) where

import System.Console.Hawk.Explicit.Type
import Control.Monad
import Data.List
import Data.Maybe

data Complete
    = CompleteValue String
    | CompleteFile String FilePath
    | CompleteDir String FilePath
      deriving (Eq,Ord)

instance Show Complete where
    show (CompleteValue a) = "VALUE " ++ a
    show (CompleteFile a b) = "FILE " ++ a ++ " " ++ b
    show (CompleteDir a b) = "DIR " ++ a ++ " " ++ b

    showList xs = showString $ unlines (map show xs)

prepend :: String -> Complete -> Complete
prepend a (CompleteFile b c) = CompleteFile (a++b) c
prepend a (CompleteDir b c) = CompleteDir (a++b) c
prepend a (CompleteValue b) = CompleteValue (a++b)

complete
    :: Mode a
    -> [String]
    -> (Int,Int)
    -> [Complete]

complete mode_ args_ (i,_) = nub $ followArgs mode args now
    where
        (seen,next) = splitAt i args_
        now = head $ next ++ [""]
        (mode,args) = followModes mode_ seen

followModes :: Mode a -> [String] -> (Mode a, [String])
followModes m (x:xs) | Just m2 <- pickBy modeNames x $ modeModes m =
    followModes m2 xs
followModes m xs = (m,xs)

pickBy :: (a -> [String]) -> String -> [a] -> Maybe a
pickBy f name xs = find (\x -> name `elem` f x) xs `mplus`
                   find (\x -> any (name `isPrefixOf`) (f x)) xs

followArgs :: Mode a -> [String] -> (String -> [Complete])
followArgs m = first
    where
        first [] = expectArgFlagMode (modeModes m) (argsPick 0) (modeFlags m)
        first xs = norm 0 xs

        norm i [] = expectArgFlag (argsPick i) (modeFlags m)
        norm i ("--":xs) = expectArg $ argsPick (i + length xs)
        norm i (('-':'-':x):xs) | null b, flagInfo flg == FlagReq =
                                    val i flg xs
                                | otherwise = norm i xs
            where (a,b) = break (== '=') x
                  flg = getFlag a
        norm i (('-':x:y):xs) = case flagInfo flg of
            FlagReq | null y -> val i flg xs
                    | otherwise -> norm i xs
            FlagOpt{} -> norm i xs
            _ | "=" `isPrefixOf` y -> norm i xs
              | null y -> norm i xs
              | otherwise -> norm i (('-':y):xs)
            where flg = getFlag [x]
        norm i (x:xs) = norm (i+1) xs

        val i flg [] = expectVal flg
        val i flg (x:xs) = norm i xs

        argsPick i =
            let (lst,end) = modeArgs m
            in if i < length lst then Just $ lst !! i else end

        getFlag x =
            fromMaybe (flagNone [] id "") $ pickBy flagNames x $ modeFlags m

expectArgFlagMode :: [Mode a] -> Maybe (Arg a) -> [Flag a] -> String -> [Complete]
expectArgFlagMode mode arg flag x =
    (if "-" `isPrefixOf` x then [] else expectMode mode x) ++
    expectArgFlag arg flag x

expectArgFlag :: Maybe (Arg a) -> [Flag a] -> String -> [Complete]
expectArgFlag arg flag x
    | "-" `isPrefixOf` x =
        expectFlag flag x ++ [CompleteValue "-" | x == "-", isJust arg]
    | otherwise = expectArg arg x ++ expectFlag flag x

expectMode :: [Mode a] -> String -> [Complete]
expectMode mode = expectStrings (map modeNames mode)

expectArg :: Maybe (Arg a) -> String -> [Complete]
expectArg Nothing x = []
expectArg (Just arg) x = expectFlagHelp (argType arg) x

expectFlag :: [Flag a] -> String -> [Complete]
expectFlag flag x
    | (a,_:b) <- break (== '=') x = case pickBy (map f . flagNames) a flag of
        Nothing -> []
        Just flg -> map (prepend (a ++ "=")) $ expectVal flg b
    | otherwise = expectStrings (map (map f . flagNames) flag) x
    where f x = "-" ++ ['-' | length x > 1] ++ x

expectVal :: Flag a -> String -> [Complete]
expectVal flg = expectFlagHelp (flagType flg)

expectStrings :: [[String]] -> String -> [Complete]
expectStrings xs x =
    map CompleteValue $ concatMap (take 1 . filter (x `isPrefixOf`)) xs

expectFlagHelp :: FlagHelp -> String -> [Complete]
expectFlagHelp typ x = case typ of
    "FILE" -> [CompleteFile "" x]
    "DIR" -> [CompleteDir "" x]
    "FILE/DIR" -> [CompleteFile "" x, CompleteDir "" x]
    "DIR/FILE" -> [CompleteDir "" x, CompleteFile "" x]
    '[':s | "]" `isSuffixOf` s -> expectFlagHelp (init s) x
    _ -> []
