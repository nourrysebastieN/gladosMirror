
module System.Console.Hawk.Text(TextFormat(..), defaultWrap, Text(..), showText) where

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import System.Console.Hawk.Default

defaultWrap :: TextFormat
defaultWrap = Wrap 80

data TextFormat = HTML
                | Wrap Int
                  deriving (Read,Show,Eq,Ord)

instance Default TextFormat where def = defaultWrap

data Text = Line String
          | Cols [String]

instance Show Text where
    showList = showString . showText defaultWrap
    show x = showText defaultWrap [x]

showText :: TextFormat -> [Text] -> String
showText HTML = showHTML
showText (Wrap x) = showWrap x

showWrap :: Int -> [Text] -> String
showWrap width xs = unlines $ concatMap f xs
    where
        cs :: [(Int,[Int])]
        cs = map (\x -> (fst $ head x, map maximum $ transpose $ map snd x)) $
                groupBy ((==) `on` fst) $ sortBy (compare `on` fst)
                [(length x, map length $ init x) | Cols x <- xs]
        pad n x = x ++ replicate (n - length x) ' '

        f (Line x) = map (a++) $ wrap1 (width - length a) b
            where (a,b) = span isSpace x

        f (Cols xs) =
            concat (zipWith pad ys xs ++ [z1]) : map (replicate n ' '++) zs
            where ys = fromJust $ lookup (length xs) cs
                  n = sum ys + length (takeWhile isSpace $ last xs)
                  z1:zs = wrap1 (width - n) (last xs)

wrap1 :: Int -> String -> [String]
wrap1 width x = ["" | null res] ++ res
    where res = wrap width x

wrap :: Int -> String -> [String]
wrap width = concatMap (combine . split) . lines
    where
        split :: String -> [(String,Int)]
        split "" = []
        split x = (a,length c) : split d
            where (a,b) = break isSpace x
                  (c,d) = span isSpace b

        combine :: [(String,Int)] -> [String]
        combine ((a,b):(c,d):xs) | length a + b + length c < width =
            combine $ (a ++ replicate b ' ' ++ c,d):xs
        combine (x:xs) = fst x : combine xs
        combine [] = []

showHTML :: [Text] -> String
showHTML xs = unlines $
    ["<table class='cmdargs'>"] ++
    map f xs ++
    ["</table>"]
    where
        maxCols = maximum [length x | Cols x <- xs]

        f (Line x) = tr $ td maxCols x
        f (Cols xs) =
            tr $ concatMap (td 1) (init xs)
            ++ td (maxCols + 1 - length xs) (last xs)

        tr x = "<tr>" ++ x ++ "</tr>"
        td cols x = "<td"
            ++ (if cols == 1 then "" else " colspan='" ++ show cols ++ "'")
            ++ (if null styles then "" else " style='" ++ unwords styles++"'")
            ++ ">" ++ if null b then "&nbsp;" else concatMap esc b ++ "</td>"
            where (a,b) = span isSpace x

                  isFlag = take 1 b == "-"
                  styles =
                        [ "padding-left:" ++ show (length a) ++ "ex;" | a/=""]
                        ++ [ "white-space:nowrap;" | isFlag ]

        esc '&' = "&amp;"
        esc '>' = "&gt;"
        esc '<' = "&lt;"
        esc '\n' = "<br />"
        esc x = [x]
