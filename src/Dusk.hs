
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}

module Dusk where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Char
import Data.Either

import Debug.Trace

import Dawn
import Monoparsec (Tok(..), fromToken, toRange)
import Monoparsec.Message as Msg

data Type_
    = Simple_ String
    | Complex_ String [Type_]

instance Show Type_ where
    show (Simple_ s) = s
    show (Complex_ s l) = s <> " " <> (intercalate " " $ map show l)

instance Eq Type_ where
    (==) (Simple_ a) (Simple_ b) = a == b
    (==) (Complex_ a b) (Complex_ c d) = a == c && b == d
    (==) (Simple_ "...") (Simple_ _) = True
    (==) (Simple_ _) (Simple_ "...") = True
    (==) _ _ = False

type S = (Tok String, [String], [(String, [String])])
type F = (Tok String, [Type_], Type_)
type E = (Tok String, [([Literal], Expression)])

data Prog = Prog
    {
        structs :: [S],
        funcs :: [F],
        entries :: [E]
    }
    deriving (Show)

displayS :: S -> IO()
displayS (n, f, c) = do
    putStrLn $ (fromToken n) <> " " <> (intercalate " " f) <> " {"
    mapM_ (\(a, b) -> putStrLn $ "    " <> a <> " " <> (intercalate " " b)) c
    putStrLn "}"

displayF :: F -> IO ()
displayF (n, f, r) = putStrLn $ (fromToken n) <> " :: " <> (intercalate "," $ map show f) <> " -> " <> (show r)

displayE :: E -> IO ()
displayE (n, e) = do
    putStrLn $ (fromToken n) <> " {"
    mapM_ (\(a, b) -> putStrLn $ "    " <> (intercalate " " $ map (showLiteral) a) <> " = " <> (showExpression b)) e
    putStrLn "}"

showLiteral :: Literal -> String
showLiteral (Oblivion) = "_"
showLiteral (Boolean b) = show b
showLiteral (Integer i) = show i
showLiteral (Str s) = show s
showLiteral (Character c) = show c
showLiteral (Identifier i) = i
showLiteral (Structure n exs) = "(" <> (fromToken n) <> " " <> (intercalate " " $ map (showExpression . fromToken) exs) <> ")"

showExpression :: Expression -> String
showExpression (Literal l) = (showLiteral . fromToken) l
showExpression (Call n exs) = "(" <> (fromToken n) <> " " <> (intercalate " " $ map (showExpression . fromToken) exs) <> ")"
showExpression (Fold e) = "(fold " <> (showExpression $ fromToken e) <> ")"

displayProg :: Prog -> IO()
displayProg p = do
    putStrLn "structs:"
    mapM_ displayS (structs p)
    putStrLn "functions:"
    mapM_ displayF (funcs p)
    putStrLn "entries:"
    mapM_ displayE (entries p)

lookup3 :: Eq a => a -> [(a, b, c)] -> Maybe (a, b, c)
lookup3 _ [] = empty
lookup3 a ((a', b, c):xs) = (guard (a == a') >> (pure (a', b, c))) <|> (lookup3 a xs)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

builtin :: String -> Bool
builtin "Int" = True
builtin "Bool" = True
builtin "Char" = True
builtin "String" = True
builtin "IO" = True
builtin _ = False

checkType :: Tok Dawn.Type -> Prog -> Maybe [Message String]
checkType t@(Tok i j (Simple n)) p = prog p n <|> ((check . fromToken) n)
    where
        prog p s = case lookup3 s (structs p) of
            Just (_, f@(x:xs), _) -> Just $ [Msg.errorString (Range i (j - 1)) ("expecting " <> (show $ length f) <> " arguments to '" <> (fromToken s) <> "'")]
            _ -> Nothing
        check s = if (builtin s || lower s) then Nothing else Just $ [Msg.errorString (Range i (j - 1)) ("type '" <> s <> "' not found")]
        lower s = Data.Char.isLower $ head s
checkType t@(Tok i j (Complex n ts)) p = types <|> (prog p n) <|> ((check . fromToken) n)
    where
        types = concat <$> (sequence $ map (flip checkType p) ts)
        prog p s = case lookup3 s (structs p) of
            Just (_, f, _) -> if (length f) == (length ts)
                then Nothing
                else if (length f) < (length ts)
                    then Just $ [Msg.errorString (Range i (j - 1)) ("'" <> (fromToken s) <> "' didn't expect any more argument")]
                    else Just $ [Msg.errorString (Range i (j - 1)) ("expecting " <> (show $ ((length f) - (length ts))) <> " arguments to '" <> (fromToken s) <> "'")]
            Nothing -> Just $ [Msg.errorString (Range i (j - 1)) ("type '" <> (fromToken s) <> "' not found")]
        check s = if (builtin s || lower s) then Just $ [Msg.errorString (Range i (j - 1)) ("type '" <> s <> "' not found")] else Nothing
        lower s = Data.Char.isLower $ head s

fromConstr' :: Tok String -> Prog -> Either [Message String] S
fromConstr' (Tok i j n) p = if null $ structs p
    then Left $ [Msg.errorString (Range i (j - 1)) ("constructor '" <> n <> "' not found")]
    else foldl (\a b -> a *> (search b)) (Right (mempty, mempty, mempty)) (structs p)
    where
        search r@(t, _, c) = case lookup n c of
            Nothing -> Left $ [Msg.errorString (Range i (j - 1)) ("constructor '" <> n <> "' not found")]
            Just _ -> Right r

fromConstr :: Tok String -> Prog -> Either [Message String] String
fromConstr (Tok i j n) p = if null $ structs p
    then Left $ [Msg.errorString (Range i (j - 1)) ("constructor '" <> n <> "' not found")]
    else foldl (\a b -> a *> (search b)) (Right "") (structs p)
    where
        search (t, _, c) = case lookup n c of
            Nothing -> Left $ [Msg.errorString (Range i (j - 1)) ("constructor '" <> n <> "' not found")]
            Just _ -> (Right . fromToken) t

checkConstr :: Tok Literal -> Type_ -> Prog -> Either [Message String] Type_
checkConstr (Tok i j (Structure n exs)) t@(Complex_ n' ts) p =
    case fromConstr' n p of
        Right (_, f, cs) -> if (length f) == (length ts)
            then case lookup (fromToken n) cs of
                Nothing -> Left $ [Msg.errorString (Range i (j - 1)) ("constructor '" <> (fromToken n) <> "' not found")]
                Just [] -> Right t
                Just l -> if (length l) /= (length exs)
                    then Left $ [Msg.errorString (Range i (j - 1)) ("expecting " <> (show $ length l) <> " arguments to '" <> (fromToken n) <> "'")]
                    else
                        let z' = zip l ts
                            m' = mapMaybe (flip lookup z') l
                            e' = zip exs m'
                        in foldl (\b (e,t') -> b *> (expressionType' e t' p)) (Right t) e'
            else Left $ [Msg.errorString (Range i (j - 1)) ("expecting " <> (show $ length f) <> " arguments to '" <> (fromToken n) <> "'")]
        Left e -> Left e

literalType' :: Tok Literal -> Type_ -> Prog -> Either [Message String] Type_
literalType' (fromToken -> Oblivion) t _ = Right t
literalType' (fromToken -> (Boolean _)) _ _ = Right $ Simple_ "Bool"
literalType' (fromToken -> (Integer _)) _ _ = Right $ Simple_ "Int"
literalType' (fromToken -> (Str _)) _ _ = Right $ Simple_ "String"
literalType' (fromToken -> (Character _)) _ _ = Right $ Simple_ "Char"
literalType' (fromToken -> (Identifier _)) t _ = Right t
literalType' tok@(Tok i j (Structure _ _)) t@(Simple_ _) p = 
    case literalType_ tok p of
        Left e -> Left e
        Right t' -> Left [Msg.errorString (Range i (j - 1)) ("expected '" <> (show t) <> "' but got '" <> (show t') <> "'")]
literalType' tok@(fromToken -> (Structure n exs)) t p =
    case checkConstr tok t p of
        Left e -> Left e
        Right t' -> Right t'

expressionType' :: Tok Expression -> Type_ -> Prog -> Either [Message String] Type_
expressionType' (fromToken -> (Literal l)) t p = literalType' l t p
expressionType' (fromToken -> (Call n _)) _ p =
    case lookup3 n (funcs p) of
        Nothing -> Left $ [Msg.errorString (toRange n) ("function '" <> (fromToken n) <> "' not found")]
        Just (_, _, r) -> Right r
expressionType' (fromToken -> (Fold e)) t p = expressionType' e t p

literalType :: Tok Literal -> [(String, Type_)] -> Prog -> Either [Message String] Type_
literalType (fromToken -> Oblivion) _ _ = Right $ Simple_ "..."
literalType (fromToken -> (Boolean _)) _ _ = Right $ Simple_ "Bool"
literalType (fromToken -> (Integer _)) _ _ = Right $ Simple_ "Int"
literalType (fromToken -> (Str _)) _ _ = Right $ Simple_ "String"
literalType (fromToken -> (Character _)) _ _ = Right $ Simple_ "Char"
literalType (Tok i j (Identifier n)) table _ =
    case lookup n table of
        Nothing -> Left $ [Msg.errorString (Range i (j - 1)) ("identifier '" <> n <> "' not found")]
        Just t -> Right t
literalType t@(Tok i j (Structure n exs)) table p =
    case fromConstr n p of
        Right s -> case check of
            Right l -> Right $ Complex_ s l
            Left e -> Left e
        Left e -> Left e
    where
        pull = map (\b -> expressionType b table p) exs
        check = check_ pull
        check_ [] = Right []
        check_ ((Right a):xs) = (:) <$> (Right a) <*> (check_ xs)
        check_ ((Left a):xs) = Left a

literalType_ :: Tok Literal -> Prog -> Either [Message String] Type_
literalType_ (fromToken -> Oblivion) _ = Right $ Simple_ "..."
literalType_ (fromToken -> (Boolean _)) _ = Right $ Simple_ "Bool"
literalType_ (fromToken -> (Integer _)) _ = Right $ Simple_ "Int"
literalType_ (fromToken -> (Str _)) _ = Right $ Simple_ "String"
literalType_ (fromToken -> (Character _)) _ = Right $ Simple_ "Char"
literalType_ (Tok i j (Identifier _)) _ = Right $ Simple_ "..."
literalType_ (fromToken -> (Structure n exs)) p =
    case fromConstr n p of
        Right s -> case check of
            Right l -> Right $ Complex_ s l
            Left e -> Left e
        Left e -> Left e
    where
        pull = map (\b -> expressionType_ b p) exs
        check = check_ pull
        check_ [] = Right []
        check_ ((Right a):xs) = (:) <$> (Right a) <*> (check_ xs)
        check_ ((Left a):xs) = Left a

expressionType :: Tok Expression -> [(String, Type_)] -> Prog -> Either [Message String] Type_
expressionType (fromToken -> (Literal l)) table p = literalType l table p
expressionType (fromToken -> (Call n exs)) table p =
    case lookup3 n (funcs p) of
        Nothing -> Left $ [Msg.errorString (toRange n) ("function '" <> (fromToken n) <> "' not found")]
        Just (_, f, r) -> if (length f) == (length exs)
            then 
                let func t ei = case ei of
                        Left a -> Left a
                        Right t' -> if (t == t')
                            then Right t
                            else Left $ [Msg.errorString (toRange n) ("expected '" <> (show t) <> "' but got '" <> (show t') <> "'")]
                    t' = map (\(e,t) -> func t $ expressionType e table p) (zip exs f)
                in case (lefts t') of
                    [] -> Right r
                    l -> Left $ concat l
            else Left $ [Msg.errorString (toRange n) ("expecting " <> (show $ length f) <> " arguments to '" <> (fromToken n) <> "'")]
    where
        pull = map (\b -> expressionType b table p) exs
        check = check_ pull
        check_ [] = Right []
        check_ ((Right a):xs) = (:) <$> (Right a) <*> (check_ xs)
        check_ ((Left a):xs) = Left a
expressionType (fromToken -> (Fold e)) t p = expressionType e t p

expressionType_ :: Tok Expression -> Prog -> Either [Message String] Type_
expressionType_ (fromToken -> (Literal l)) p = literalType_ l p
expressionType_ (fromToken -> (Call n _)) p =
    case lookup3 n (funcs p) of
        Nothing -> Left $ [Msg.errorString (toRange n) ("function '" <> (fromToken n) <> "' not found")]
        Just (_, _, r) -> Right r
expressionType_ (fromToken -> (Fold e)) p = expressionType_ e p

checkLiteralType :: Tok Literal -> Type_ -> Prog -> Maybe [Message String]
checkLiteralType (fromToken -> Oblivion) _ _ = Nothing
checkLiteralType (fromToken -> (Boolean _)) (Simple_ "Bool") _ = Nothing
checkLiteralType (fromToken -> (Integer _)) (Simple_ "Int") _ = Nothing
checkLiteralType (fromToken -> (Str _)) (Simple_ "String") _ = Nothing
checkLiteralType (fromToken -> (Character _)) (Simple_ "Char") _ = Nothing
checkLiteralType (fromToken -> (Identifier _)) _ _ = Nothing
checkLiteralType tok@(Tok i j (Structure _ _)) t@(Simple_ _) p = 
    case literalType_ tok p of
        Left e -> Just e
        Right t' -> Just $ [Msg.errorString (Range i (j - 1)) ("expected '" <> (show t) <> "' but got '" <> (show t') <> "'")]
checkLiteralType tok@(Tok i j (Structure _ _)) t@(Complex_ n ts) p =
    case checkConstr tok t p of
        Left e -> Just e
        Right t' -> Nothing

getIdentifierType :: Tok Literal -> Type_ -> Prog -> [(String, Type_)]
getIdentifierType (fromToken -> (Identifier n)) t _ = [(n, t)]
getIdentifierType (fromToken -> (Structure n exs)) t@(Complex_ n' ts) p =
    case fromConstr' n p of
        Right (_, f, cs) -> case lookup (fromToken n) cs of
                Nothing -> []
                Just [] -> []
                Just l ->
                    let z' = zip f ts
                        m' = mapMaybe (flip lookup z') l
                        e' = zip exs m'
                    in foldl (\b (e,t') -> b ++ (getExpressionType e t' p)) [] e'
        Left _ -> []
getIdentifierType _ _ _ = []

getExpressionType :: Tok Expression -> Type_ -> Prog -> [(String, Type_)]
getExpressionType (fromToken -> (Literal l)) t p = getIdentifierType l t p
getExpressionType _ _ _ = []

compileDeclaration :: Tok Declaration -> Prog -> Either [Message String] Prog
compileDeclaration (fromToken -> (Struct n@(Tok s e n') f cs)) p =
    case lookup3 n (structs p) of
        Nothing -> case check f cs of
            Right _ -> Right $ p { structs = (curry3 id n (map fromToken f) ((transform) cs)) : structs p }
            Left e -> Left e
        Just ((Tok s' e' _), _, _) -> Left $ [addSuggestion (err n' s e) (sugg n' s' e')]
    where
        sugg n i j = Msg.infoString (Range i (j - 1)) ("'" <> n <> "' declared here")
        err n i j = Msg.errorString (Range i (j - 1)) ("'" <> n <> "' already defined")
        transform cs = map (t . fromToken) cs
        t (a, b) = (fromToken a, map fromToken b)

        check fs cs = checkConstructor cs *> (foldl (\b a -> b *> ((checkField fs) . fromToken) a) (Right ()) cs)
        checkConstructor cs =
            let cs' = map (fst . fromToken) cs
                convert e = (fromJust $ find (`elem` cs') e, e)
                test e l = case elemIndices e l of
                    [x] -> Right ()
                    (a:b:_) -> Left $ [err (l !! a) (l !! b)]
                err (Tok i j c) (Tok i' j' _) = addSuggestion
                    (Msg.errorString (Range i' (j' - 1)) ("constructor '" <> c <> "' already defined"))
                    (Msg.infoString (Range i (j - 1)) ("constructor '" <> c <> "' defined here"))
            in foldl (\b a -> b *> (test a cs')) (Right ()) cs'
        checkField fs (_,fs') = case find (`notElem` fs) fs' of
            Nothing -> Right ()
            Just (Tok i j f) -> Left $ [Msg.errorString (Range i (j - 1)) ("field '" <> f <> "' not found")]

compileDeclaration (fromToken -> (Function n@(Tok s e n') ps r)) p =
    case lookup3 n (funcs p) of
        Nothing -> case check ps r of
            Nothing -> Right $ p { funcs = ((n, map (transform) ps, (transform) r) : funcs p) }
            Just e -> Left e
        Just ((Tok s' e' _), _, _) -> Left $ [addSuggestion (err n' s e) (sugg n' s' e')]
    where
        sugg n i j = Msg.infoString (Range i (j - 1)) (n <> " declared here")
        err n i j = Msg.errorString (Range i (j - 1)) (n <> " already defined")

        check ps r = (foldl (\b a -> b <|> (checkType a p)) Nothing ps) <|> (checkType r p)
        transform (fromToken -> (Simple n)) = Simple_ $ fromToken n
        transform (fromToken -> (Complex n ts)) = Complex_ (fromToken n) (map transform ts)

compileDeclaration (fromToken -> (FunctionSpec n@(Tok i j n') es)) p =
    case lookup3 n (funcs p) of
        Nothing -> Left $ [Msg.errorString (Range i (j - 1)) ("function '" <> n' <> "' not found")]
        Just (_, f, r) -> case concat (sequence $ mapMaybe (check f r) es) of
            [] -> Right $ p { entries = (n, (map (transform . fromToken) es)) : entries p }
            l -> Left l
    where
        check f r (Tok i j entry) = (checkArg i j f (fst entry)) <|> (checkReturn i j f r entry)
        checkArg i j f ls = if (length f) == (length ls)
            then foldl (\b (l,t) -> b <|> checkLiteralType l t p) (Nothing) (zip ls f)
            else Just $ [Msg.errorString (Range i (j - 1)) ("expecting " <> (show $ length f) <> " arguments to '" <> (fromToken n) <> "'")]
        checkReturn i j f r (ls,e) = 
            let t' = map (\(l,t) -> literalType' l t p) (zip ls f)
            in case (lefts t') of
                    [] ->
                        let t'' = zip ls f
                            table = concat $ map (\(l,t) -> getIdentifierType l t p) t''
                        in case expressionType e table p of
                            Left l -> case expressionType' e r p of
                                Left _ -> Just l
                                Right r' -> if (r == r')
                                    then Nothing
                                    else Just $ [Msg.errorString (Range i (j - 1)) ("expected '" <> (show r) <> "' but got '" <> (show r') <> "'")]
                            Right r' -> if (r == r')
                                then Nothing
                                else case expressionType' e r p of
                                    Left _ -> Just $ [Msg.errorString (Range i (j - 1)) ("expected '" <> (show r) <> "' but got '" <> (show r') <> "'")]
                                    Right r' -> if (r == r')
                                        then Nothing
                                        else Just $ [Msg.errorString (Range i (j - 1)) ("expected '" <> (show r) <> "' but got '" <> (show r') <> "'")]
                    l -> Just $ concat l

        transform entry = (map fromToken $ fst entry, fromToken $ snd entry)

defaultProg :: Prog
defaultProg = Prog
    [

    ]
    [
        (Tok 0 0 "+", [Simple_ "Int", Simple_ "Int"], Simple_ "Int"),
        (Tok 0 0 "-", [Simple_ "Int", Simple_ "Int"], Simple_ "Int"),
        (Tok 0 0 "*", [Simple_ "Int", Simple_ "Int"], Simple_ "Int"),
        (Tok 0 0 "/", [Simple_ "Int", Simple_ "Int"], Simple_ "Int"),
        (Tok 0 0 "print", [Simple_ "Int"], Simple_ "IO")
    ]
    [

    ]

-- (Tok String, [Type_], Type_)

compile :: [Tok Declaration] -> Either [Message String] Prog
compile l = foldl (\a b -> a >>= compileDeclaration b) (Right defaultProg) l
