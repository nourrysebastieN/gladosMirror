{-# LANGUAGE ExistentialQuantification #-}

module Data.Hawk.Any where

import Control.Exception
import Control.Monad.Trans.State
import qualified Data.Data as D
import Data.Data hiding (toConstr, typeOf, dataTypeOf)
import Data.List
import Data.Maybe
import System.IO.Unsafe

readTupleType :: String -> Maybe Int
readTupleType x | "(" `isPrefixOf` x && ")" `isSuffixOf` x && all (== ',') y = Just $ length y
                | otherwise = Nothing
    where y = init $ tail x

try1 :: a -> Either SomeException a
try1 = unsafePerformIO . try . evaluate

data Any = forall a . Data a => Any a

type AnyT t = Any

instance Show Any where
    show = show . typeOf

fromAny :: Typeable a => Any -> a
fromAny (Any x) = case D.cast x of
    Just y -> y
    ~(Just y) -> error $ "Data.Hawk.Any.fromAny: Failed to extract any, got " ++
                         show (D.typeOf x) ++ ", wanted " ++ show (D.typeOf y)

cast :: Typeable a => Any -> Maybe a
cast (Any x) = D.cast x

toConstr :: Any -> Constr
toConstr (Any x) = D.toConstr x

typeOf :: Any -> TypeRep
typeOf (Any x) = D.typeOf x

dataTypeOf :: Any -> DataType
dataTypeOf (Any x) = D.dataTypeOf x

isAlgType :: Any -> Bool
isAlgType = D.isAlgType . dataTypeOf

typeShell :: Any -> String
typeShell = tyconUQname . typeShellFull

typeShellFull :: Any -> String
typeShellFull = tyConName . typeRepTyCon . typeOf

typeName :: Any -> String
typeName = show . typeOf

ctor :: Any -> String
ctor = showConstr . toConstr

fields :: Any -> [String]
fields = constrFields . toConstr

children :: Any -> [Any]
children (Any x) = gmapQ Any x

compose0 :: Any -> String -> Any
compose0 x c | either (const False) (== c) $ try1 $ ctor x = x
compose0 (Any x) c = Any $ fromConstrB err y `asTypeOf` x
    where Just y = readConstr (D.dataTypeOf x) c
          err = error $ "Data.Hawk.Any: Undefined field inside compose0, " ++ c ++ " :: " ++ show (Any x)

recompose :: Any -> [Any] -> Any
recompose (Any x) cs | null s = Any $ res `asTypeOf` x
                     | otherwise = err
    where (res,s) = runState (fromConstrM field $ D.toConstr x) cs

          field :: Data d => State [Any] d
          field = do cs <- get
                     if null cs then err else do
                         put $ tail cs
                         return $ fromAny $ head cs

          err = error $ "Data.Hawk.Any.recompose: Incorrect number of children to recompose, " ++
                        ctor (Any x) ++ " :: " ++ show (Any x) ++ ", expected " ++ show (arity $ Any x) ++
                        ", got " ++ show (length cs)

ctors :: Any -> [String]
ctors = map showConstr . dataTypeConstrs . dataTypeOf

decompose :: Any -> (String,[Any])
decompose x = (ctor x, children x)

arity = length . children

compose :: Any -> String -> [Any] -> Any
compose t c xs = recompose (compose0 t c) xs

getField :: String -> Any -> Any
getField lbl x = fromMaybe (error $ "getField: Could not find field " ++ show lbl) $
    lookup lbl $ zip (fields x) (children x)

setField :: (String,Any) -> Any -> Any
setField (lbl,child) parent
    | lbl `notElem` fs = error $ "setField: Could not find field " ++ show lbl
    | otherwise = recompose parent $ zipWith (\f c -> if f == lbl then child else c) fs cs
    where
        fs = fields parent
        cs = children parent
