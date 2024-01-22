{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Yay
-}

module System.Console.Hawk.Explicit.Type where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Semigroup hiding (Arg)
import Prelude

type Name = String

type Help = String

type FlagHelp = String

parseBool :: String -> Maybe Bool
parseBool s | ls `elem` true  = Just True
            | ls `elem` false = Just False
            | otherwise = Nothing
    where
        ls = map toLower s
        true = ["true","yes","on","enabled","1"]
        false = ["false","no","off","disabled","0"]

data Group a = Group
    {groupUnnamed :: [a]
    ,groupHidden :: [a]
    ,groupNamed :: [(Help, [a])]
    } deriving Show

instance Functor Group where
    fmap f (Group a b c) = Group (map f a) (map f b) (map (second $ map f) c)

instance Semigroup (Group a) where
    Group x1 x2 x3 <> Group y1 y2 y3 = Group (x1++y1) (x2++y2) (x3++y3)

instance Monoid (Group a) where
    mempty = Group [] [] []
    mappend = (<>)

fromGroup :: Group a -> [a]
fromGroup (Group x y z) = x ++ y ++ concatMap snd z

toGroup :: [a] -> Group a
toGroup x = Group x [] []

data Mode a = Mode
    {modeGroupModes :: Group (Mode a)
    ,modeNames :: [Name]
    ,modeValue :: a
    ,modeCheck :: a -> Either String a
    ,modeReform :: a -> Maybe [String]
    ,modeExpandAt :: Bool

    ,modeHelp :: Help
    ,modeHelpSuffix :: [String]
    ,modeArgs :: ([Arg a], Maybe (Arg a))
    ,modeGroupFlags :: Group (Flag a)
    }

modeModes :: Mode a -> [Mode a]
modeModes = fromGroup . modeGroupModes

modeFlags :: Mode a -> [Flag a]
modeFlags = fromGroup . modeGroupFlags

data FlagInfo
    = FlagReq
    | FlagOpt String
    | FlagOptRare String
    | FlagNone
      deriving (Eq,Ord,Show)

fromFlagOpt :: FlagInfo -> String
fromFlagOpt (FlagOpt x) = x
fromFlagOpt (FlagOptRare x) = x

type Update a = String -> a -> Either String a

data Flag a = Flag
    {flagNames :: [Name]
    ,flagInfo :: FlagInfo
    ,flagValue :: Update a
    ,flagType :: FlagHelp
    ,flagHelp :: Help
    }

data Arg a = Arg
    {argValue :: Update a
    ,argType :: FlagHelp
    ,argRequire :: Bool
    }

checkMode :: Mode a -> Maybe String
checkMode x = msum
    [checkNames "modes" $ concatMap modeNames $ modeModes x
    ,msum $ map checkMode $ modeModes x
    ,checkGroup $ modeGroupModes x
    ,checkGroup $ modeGroupFlags x
    ,checkNames "flag names" $ concatMap flagNames $ modeFlags x]
    where
        checkGroup :: Group a -> Maybe String
        checkGroup x = msum
            [check "Empty group name" $ not $ any (null . fst) $ groupNamed x
            ,check "Empty group contents"
            $ not $ any (null . snd) $ groupNamed x]

        checkNames :: String -> [Name] -> Maybe String
        checkNames msg xs = check "Empty names" (not (any null xs)) `mplus` do
            bad <- listToMaybe $ xs \\ nub xs
            let dupe = filter (== bad) xs
            return $ "Sanity check failed, multiple "
                ++ msg ++ ": " ++ unwords (map show dupe)

        check :: String -> Bool -> Maybe String
        check msg True = Nothing
        check msg False = Just msg

class Remap m where

    remap :: (a -> b)
          -> (b -> (a, a -> b))
          -> m a -> m b

remap2 :: Remap m => (a -> b) -> (b -> a) -> m a -> m b
remap2 f g = remap f (\x -> (g x, f))

instance Remap Mode where
    remap f g x = x
        {modeGroupModes = fmap (remap f g) $ modeGroupModes x
        ,modeValue = f $ modeValue x
        ,modeCheck = \v -> let (a,b) = g v in fmap b $ modeCheck x a
        ,modeReform = modeReform x . fst . g
        ,modeArgs = (fmap (remap f g) *** fmap (remap f g)) $ modeArgs x
        ,modeGroupFlags = fmap (remap f g) $ modeGroupFlags x}

instance Remap Flag where
    remap f g x = x{flagValue = remapUpdate f g $ flagValue x}

instance Remap Arg where
    remap f g x = x{argValue = remapUpdate f g $ argValue x}

remapUpdate :: (a -> b) -> (b -> (a, a -> b)) -> Update a -> Update b
remapUpdate f g upd = \s v -> let (a,b) = g v in fmap b $ upd s a

modeEmpty :: a -> Mode a
modeEmpty x =
    Mode mempty [] x Right (const Nothing) True "" [] ([],Nothing) mempty

mode :: Name -> a -> Help -> Arg a -> [Flag a] -> Mode a
mode name value help arg flags =
    (modeEmpty value){
        modeNames=[name]
        , modeHelp=help
        , modeArgs=([],Just arg)
        , modeGroupFlags=toGroup flags}

modes :: String -> a -> Help -> [Mode a] -> Mode a
modes name value help xs =
    (modeEmpty value){
        modeNames=[name]
        , modeHelp=help
        , modeGroupModes=toGroup xs}

flagNone :: [Name] -> (a -> a) -> Help -> Flag a
flagNone names f help = Flag names FlagNone upd "" help
    where upd _ x = Right $ f x

flagOpt :: String -> [Name] -> Update a -> FlagHelp -> Help -> Flag a
flagOpt def names upd typ help = Flag names (FlagOpt def) upd typ help

flagReq :: [Name] -> Update a -> FlagHelp -> Help -> Flag a
flagReq names upd typ help = Flag names FlagReq upd typ help

flagArg :: Update a -> FlagHelp -> Arg a
flagArg upd typ = Arg upd typ False

flagBool :: [Name] -> (Bool -> a -> a) -> Help -> Flag a
flagBool names f help = Flag names (FlagOptRare "") upd "" help
    where
        upd s x = case if s == "" then Just True else parseBool s of
            Just b -> Right $ f b x
            Nothing -> Left "expected boolean value (true/false)"
