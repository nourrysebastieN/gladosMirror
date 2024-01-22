{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Console.Hawk.Explicit.Help(HelpFormat(..), helpText) where

import System.Console.Hawk.Explicit.Type
-- import System.Console.Hawk.Explicit.Complete
import System.Console.Hawk.Text
import System.Console.Hawk.Default
import Data.List
import Data.Maybe

data HelpFormat
    = HelpFormatDefault
    | HelpFormatOne
    | HelpFormatAll
    -- | HelpFormatBash
    -- | HelpFormatZsh
      deriving (Read,Show,Enum,Bounded,Eq,Ord)

instance Default HelpFormat where def = HelpFormatDefault

instance Show (Mode a) where
    show = show . helpTextDefault

instance Show (Flag a) where
    show = show . helpFlag

instance Show (Arg a) where
    show = show . argType

helpText :: [String] -> HelpFormat -> Mode a -> [Text]
helpText pre HelpFormatDefault x = helpPrefix pre ++ helpTextDefault x
helpText pre HelpFormatOne x = helpPrefix pre ++ helpTextOne x
helpText pre HelpFormatAll x = helpPrefix pre ++ helpTextAll x

helpPrefix :: [String] -> [Text]
helpPrefix xs = map Line xs ++ [Line "" | not $ null xs]

helpTextDefault :: Mode a -> [Text]
helpTextDefault x = if length all > 40 then one else all
    where all = helpTextAll x
          one = helpTextOne x

helpTextAll :: Mode a -> [Text]
helpTextAll = disp . push ""
    where
        disp m =
            uncurry (++) (helpTextMode m)
            ++ concatMap (\x -> Line "" : disp x) (modeModes m)
        push s m = m{modeNames = map (s++) $ modeNames m
                    ,modeGroupModes = fmap (push s2) $ modeGroupModes m}
            where s2 = s ++ concat (take 1 $ modeNames m) ++ " "

helpTextOne :: Mode a -> [Text]
helpTextOne m = pre ++ ms ++ suf
    where
        (pre,suf) = helpTextMode m
        ms = 
            space
            $ [Line "Commands:" | not $ null $ groupUnnamed $ modeGroupModes m]
            ++ helpGroup f (modeGroupModes m)
        f m = return $ cols [concat $ take 1 $ modeNames m, ' ' : modeHelp m]

helpTextMode :: Mode a -> ([Text], [Text])
helpTextMode x@Mode{modeGroupFlags=flags,modeGroupModes=modes} = (pre,suf)
    where
        pre = [Line $ unwords $ take 1 (modeNames x) ++
                  ["[COMMAND] ..." | notNullGroup modes] ++
                  ["[OPTIONS]" | not $ null $ fromGroup flags] ++
                  helpArgs (modeArgs x)] ++
              [Line $ "  " ++ modeHelp x | not $ null $ modeHelp x]
        suf = space
                  ([Line "Flags:" | mixedGroup flags] ++
                   helpGroup helpFlag (modeGroupFlags x)) ++
              space (map Line $ modeHelpSuffix x)

helpGroup :: (a -> [Text]) -> Group a -> [Text]
helpGroup f xs = concatMap f (groupUnnamed xs) ++ concatMap g (groupNamed xs)
    where g (a,b) = Line (a ++ ":") : concatMap f b

helpArgs :: ([Arg a], Maybe (Arg a)) -> [String]
helpArgs (ys,y) =
    [
        ['['|o]
        ++ argType x
        ++ [']'|o] | (i,x) <- zip [0..] xs, let o = False && req <= i]
    where
        xs = ys ++ maybeToList y
        req = maximum $ 0 : [i | (i,x) <- zip [1..] xs, argRequire x]

helpFlag :: Flag a -> [Text]
helpFlag x =
    [cols [
        unwords $ map ("-"++) a2
        , unwords $ map ("--"++) b2
        , ' ' : flagHelp x
        ]
    ]
        where
            (a,b) = partition ((==) 1 . length) $ flagNames x
            (a2,b2) = if null b then (add a opt, b) else (a, add b opt)
            add x y = if null x then x else (head x ++ y) : tail x
            hlp = if null (flagType x) then "ITEM" else flagType x
            opt = case flagInfo x of
                FlagReq -> '=' : hlp
                FlagOpt x -> "[=" ++ hlp ++ "]"
                _ -> ""

cols :: [String] -> Text
cols (x:xs) = Cols $ ("  "++x) : map (' ':) xs

space :: [Text] -> [Text]
space xs = [Line "" | not $ null xs] ++ xs

nullGroup :: Group a -> Bool
nullGroup x = null (groupUnnamed x) && null (groupNamed x)

notNullGroup :: Group a -> Bool
notNullGroup = not . nullGroup

mixedGroup :: Group a -> Bool
mixedGroup x = not $ null (groupUnnamed x) || null (groupNamed x)
