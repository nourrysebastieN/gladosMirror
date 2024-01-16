{-# LANGUAGE PatternGuards, ScopedTypeVariables, ExistentialQuantification, DeriveDataTypeable #-}
{-# OPTIONS_GHC -O0 #-}

module System.Console.Hawk.Annotate(

    Capture(..), Any(..), fromCapture, defaultMissing,

    capture, (!), value, build, Annotate((:!))
    ) where

import Control.Monad
import Control.Monad.Trans.State
import Data.Data(Data, constrFields,Typeable)
import Data.List
import Data.Maybe
import Data.IORef
import Data.Hawk.Any
import System.IO.Unsafe
import Control.Exception

infixl 2 !
infix 3 :!

data Capture a
    = Compose a (Capture a)
    | Value Any
    | Miss Any
    | Ctor Any [Capture a]
      deriving Show

instance Functor Capture where
    fmap f (Compose a x) = Compose (f a) $ fmap f x
    fmap f (Value x) = Value x
    fmap f (Miss x) = Miss x
    fmap f (Ctor x xs) = Ctor x $ map (fmap f) xs

fromCapture :: Capture a -> Any
fromCapture (Compose _ x) = fromCapture x
fromCapture (Value x) = x
fromCapture (Miss x) = x
fromCapture (Ctor x _) = x

defaultMissing :: Capture a -> Capture a
defaultMissing x = evalState (f Nothing Nothing x) []
    where
        f ctor field (Compose a x) = fmap (Compose a) $ f ctor field x
        f ctor field (Value x) = return $ Value x
        f (Just ctor) (Just field) (Miss x) = do
            s <- get
            return $ head $
                [x2 | (ctor2,field2,x2) <- s, typeOf ctor == typeOf ctor2, field == field2] ++
                err ("missing value encountered, no field for " ++ field ++ " (of type " ++ show x ++ ")")
        f _ _ (Miss x) = err $ "missing value encountered, but not as a field (of type " ++ show x ++ ")"
        f _ _ (Ctor x xs) | length (fields x) == length xs = do
            ys <- zipWithM (g x) (fields x) xs
            return $ Ctor (recompose x $ map fromCapture ys) ys
        f _ _ (Ctor x xs) = fmap (Ctor x) $ mapM (f Nothing Nothing) xs

        g ctor field x = do
            y <- f (Just ctor) (Just field) x
            modify ((ctor,field,y):)
            return y

        err x = error $ "System.Console.Hawk.Annotate.defaultMissing, " ++ x

{-
Notes On Purity

There is a risk that things that are unsafe will be inlined. That can generally be
removed by NOININE on everything.

There is also a risk that things get commoned up. For example:

foo = trace "1" 1
bar = trace "1" 1
main = do
    evaluate foo
    evaluate bar

Will print "1" only once, since foo and bar share the same pattern. However, if
anything in the value is a lambda they are not seen as equal. We exploit this by
defining const_ and id_ as per this module.

Now anything wrapped in id_ looks different from anything else.
-}

{-
The idea is to keep a stack of either continuations, or values
If you encounter 'many' you become a value
If you encounter '&=' you increase the continuation
-}

-- {-# NOINLINE ref #-}
-- ref :: IORef [Either (Capture Any -> Capture Any) (Capture Any)]
-- ref = unsafePerformIO $ newIORef []

-- push = modifyIORef ref (Left id :)
-- pop = do x:xs <- readIORef ref; writeIORef ref xs; return x
-- change f = modifyIORef ref $ \x -> case x of Left g : rest -> f g : rest ; _ -> error "Internal error in Capture"
-- add f = change $ \x -> Left $ x . f
-- set x = change $ \f -> Right $ f x

-- {-# NOINLINE many #-}
-- many :: Data val => [val] -> val
-- many xs = unsafePerformIO $ do
--     ys <- mapM (force . Any) xs
--     set $ Many ys
--     return $ head xs

-- {-# NOINLINE addAnn #-}
-- addAnn :: (Data val, Data a) => val -> a -> val
-- addAnn x y = unsafePerformIO $ do
--     add (Compose $ Any y)
--     evaluate x
--     return x

-- {-# NOINLINE capture #-}
-- capture :: (Data val, Data a) => val -> Capture a
-- capture x = unsafePerformIO $ fmap (fmap fromAny) $ force $ Any x

-- force :: Any -> IO (Capture Any)
-- force x@(Any xx) = do
--     push
--     res <- try $ evaluate xx
--     y <- pop
--     case y of
--         _ | Left (_ :: RecConError) <- res -> return $ Miss x
--         Right r -> return r
--         Left f | not $ isAlgType x -> return $ f $ Value x
--                | otherwise -> do
--             cs <- mapM force $ children x
--             return $ f $ Ctor x cs

-- {-# INLINE (&=) #-}
-- (&=) :: (Data val, Data a) => val -> a -> val
-- (&=) x y = addAnn (id_ x) (id_ y)

-- {-# INLINE id_ #-}
-- id_ :: a -> a
-- id_ x = case unit of () -> x
--     where unit = reverse "" `seq` ()

data Annotate a
    = forall c f . (Data c, Data f) => (c -> f) :! f
    | HCompose a (Annotate a)
    | HValue Any
    | HCtor Any [Annotate a]
      deriving Typeable

(!) :: Annotate a -> a -> Annotate a
(!) = flip HCompose

value :: Data val => val -> Annotate a
value = HValue . Any

build :: Data a => a -> [Annotate b] -> Annotate b
build a b = HCtor (Any a) b

capture :: Show a => Annotate a -> IO (Capture a)
capture (HCompose a x) = Compose a <$> (capture x)
capture (HValue x) = pure $ Value x
capture (_ :! c) = pure $ Value $ Any c
capture (HCtor x xs) = inds >>= construct
    where
        construct i = when ((not . null) (rep i)) (err i) >> (Ctor <$> x2 <*> xs2)
        x2 = recompose x <$> (map fromCapture) <$> xs2
        xs2 = sequence $ [fromMaybe (Miss c) <$> (lookup i <$> is) | let is = zip <$> inds <*> (sequence $ map capture xs), (i,c) <- zip [0..] $ children x]
        inds = zipWith fromMaybe [0..] <$> (sequence . map (fieldIndex x)) xs
        rep i = i\\ nub i
        conv i = constrFields (toConstr x) !! i
        err i = fail $ "System.Console.Hawk.capture: Clone field detected: " <> (intercalate ", " $ map conv (rep i))

fieldIndex :: Any -> Annotate a -> IO (Maybe Int)
fieldIndex ctor (HCompose a x) = fieldIndex ctor x
fieldIndex ctor (f :! _) = res >>= alt
    where c = recompose ctor [Any $ throwIndex i `asTypeOf` x | (i,Any x) <- zip [0..] (children ctor)]
          res = catchInt $ f $ fromAny c
          alt i = unless (isJust i) (fail $ "Couldn't resolve field for " ++ show ctor) >> return i
fieldIndex _ _ = pure Nothing

data Index = Index Int deriving (Show, Typeable)
instance Exception Index

throwIndex :: Int -> a
throwIndex i = throw (Index i)

catchInt :: a -> IO (Maybe Int)
catchInt x = try (evaluate x) >>= func
    where
        func y = return $ case y of
                        Left (Index z) -> Just z
                        _ -> Nothing
