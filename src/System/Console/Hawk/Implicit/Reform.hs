{-# LANGUAGE RecordWildCards #-}

module System.Console.Hawk.Implicit.Reform(reform) where

import System.Console.Hawk.Implicit.Local
import System.Console.Hawk.Implicit.Type
import System.Console.Hawk.Verbosity

import Data.Hawk.Any
import Data.List
import Data.Maybe

reform :: Prog_ -> Hawk Any -> Maybe [String]
reform Prog_{..} Hawk{..} = Just $
    f "help" progHelpArg (isJust hawkHelp) ++
    f "version" progVersionArg (isJust hawkVersion) ++
    f "verbose" (fst progVerbosityArgs) (hawkVerbosity == Just Loud) ++
    f "quiet" (snd progVerbosityArgs) (hawkVerbosity == Just Quiet)
    where
        f ex (Just x) True = pickArg $ builtinNames x ++ [ex]
        f _ _ _ = []

pickArg :: [String] -> [String]
pickArg xs = case partition ((==) 1 . length) xs of
    (_, x:_) -> ["--" ++ x]
    (x:_, _) -> ["-" ++ x]
    _ -> []

{-

data Prog_ = Prog_
    {progModes :: [Mode_]
    ,progSummary :: Maybe [String]
    ,progProgram :: String
    ,progHelp :: String
    ,progVerbosityArgs :: (Maybe Builtin_, Maybe Builtin_)
    ,progHelpArg :: Maybe Builtin_
    ,progVersionArg :: Maybe Builtin_
    } deriving Show
-}
