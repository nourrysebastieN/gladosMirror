{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}

module System.Console.Hawk.Helper(

    execute,

    Unknown, receive, reply, comment
    ) where

import System.Console.Hawk.Explicit.Type
import System.Console.Hawk.Explicit.SplitJoin
import System.Process
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import System.Exit
import System.IO
import System.IO.Unsafe

hOut h x = do
    hPutStrLn h x
    hFlush h

execute
    :: String
    -> Mode a
    -> [String]
    -> IO (Either String [String])
execute cmd mode args
    | "echo" == takeWhile (not . isSpace) cmd = return $ Right $ splitArgs $ drop 4 cmd
    | otherwise = withBuffering stdout NoBuffering $ do
        (Just hin, Just hout, _, _) <- createProcess (shell cmd){std_in=CreatePipe, std_out=CreatePipe}

        hSetBuffering hin LineBuffering
        hSetBuffering hout LineBuffering
        (m, ans) <- saveMode mode
        hOut hin m
        loop ans hin hout
    where
        loop ans hin hout = do
            x <- hGetLine hout
            if "Result " `isPrefixOf` x then
                return $ read $ drop 7 x
             else if "Send " `isPrefixOf` x then do
                hOut hin =<< ans (drop 5 x)
                loop ans hin hout
             else if "#" `isPrefixOf` x then do
                hOut stdout x
                loop ans hin hout
             else
                return $ Left $ "Unexpected message from program: " ++ show x

withBuffering hndl mode act = bracket
    (do old <- hGetBuffering hndl; hSetBuffering hndl mode; return old)
    (hSetBuffering hndl)
    (const act)

newtype Unknown = Unknown {fromUnknown :: Value}

receive :: IO (Mode Unknown)
receive = do
    m <- getLine
    return $ remap2 Unknown fromUnknown $ loadMode m $ \msg -> unsafePerformIO $ do
        hOut stdout $ "Send " ++ msg
        getLine

reply :: Either String [String] -> IO ()
reply x = do
    hOut stdout $ "Result " ++ show x
    exitWith ExitSuccess

comment :: String -> IO ()
comment x = hOut stdout $ "# " ++ x

data IOMap a = IOMap (IORef (Int,[(Int,a)]))

newIOMap :: IO (IOMap a)
newIOMap = fmap IOMap $ newIORef (0, [])

addIOMap :: IOMap a -> a -> IO Int
addIOMap (IOMap ref) x = atomicModifyIORef ref $ \(i,xs) -> let j = i+1 in ((j,(j,x):xs), j)

getIOMap :: IOMap a -> Int -> IO a
getIOMap (IOMap ref) i = do (_,xs) <- readIORef ref; return $ fromJust $ lookup i xs

newtype Value = Value Int

{-# NOINLINE toValue #-}
toValue :: Mode a -> Mode Value

toValue x = unsafePerformIO $ do

    mp <- newIOMap
    let embed x = unsafePerformIO $ fmap Value $ addIOMap mp x
        proj (Value x) = unsafePerformIO $ getIOMap mp x
    return $ remap2 embed proj x

saveMode :: Mode a -> IO (String, String -> IO String)
saveMode m = do
    mp <- newIOMap
    res <- add mp $ pack $ toValue m
    return $ (show res, fmap show . get mp . read)
    where
        add :: IOMap (Pack -> Pack) -> Pack -> IO Pack
        add mp x = flip transformM x $ \x -> case x of
            Func (NoShow f) -> do i <- addIOMap mp f; return $ FuncId i
            x -> return x

        get :: IOMap (Pack -> Pack) -> (Int,Pack) -> IO Pack
        get mp (i,x) = do
            f <- getIOMap mp i
            add mp $ f x

loadMode :: String -> (String -> String) -> Mode Value
loadMode x f = unpack $ rep $ read x
    where
        rep :: Pack -> Pack
        rep x = flip transform x $ \x -> case x of
            FuncId i -> Func $ NoShow $ \y -> rep $ read $ f $ show (i,y)
            x -> x

data Pack = Ctor String [(String, Pack)]
          | List [Pack]
          | Char Char
          | Int Int
          | Func (NoShow (Pack -> Pack))
          | FuncId Int
          | String String
          | None
            deriving (Show,Read)

newtype NoShow a = NoShow a
instance Show (NoShow a) where showsPrec = error "Cannot show value of type NoShow"
instance Read (NoShow a) where readsPrec = error "Cannot read value of type NoShow"

transformM, descendM :: Monad m => (Pack -> m Pack) -> Pack -> m Pack
transformM f x = f =<< descendM (transformM f) x
descendM f x = let (a,b) = uniplate x in liftM b $ mapM f a

transform, descend :: (Pack -> Pack) -> Pack -> Pack
transform f = f . descend (transform f)
descend f x = let (a,b) = uniplate x in b $ map f a

uniplate :: Pack -> ([Pack], [Pack] -> Pack)
uniplate (List xs) = (xs, List)
uniplate (Ctor x ys) = (map snd ys, Ctor x . zip (map fst ys))
uniplate x = ([], const x)

class Packer a where
    pack :: a -> Pack
    unpack :: Pack -> a

add a b = (a, pack b)
ctor x (Ctor y xs) | x == y = xs
ctor _ _ = []
get a b = unpack $ fromMaybe None $ lookup a b

instance Packer a => Packer [a] where
    pack xs = if length ys == length zs && not (null ys) then String zs else List ys
        where ys = map (pack) xs
              zs = [x | Char x <- ys]

    unpack (String xs) = unpack $ List $ map Char xs
    unpack (List xs) = map (unpack) xs
    unpack _ = []

instance (Packer a, Packer b) => Packer (a -> b) where
    pack f = Func $ NoShow $ pack . f . unpack
    unpack (Func (NoShow f)) = unpack . f . pack

instance Packer Value where
    pack (Value x) = pack x
    unpack x = Value $ unpack x

instance Packer Char where
    pack = Char
    unpack (Char x) = x
    unpack _ = ' '

instance Packer Int where
    pack = Int
    unpack (Int x) = x
    unpack _ = -1

instance (Packer a, Packer b) => Packer (a,b) where
    pack (a,b) = Ctor "(,)" [add "fst" a, add "snd" b]
    unpack x = (get "fst" y, get "snd" y)
        where y = ctor "(,)" x

instance Packer a => Packer (Maybe a) where
    pack Nothing = Ctor "Nothing" []
    pack (Just x) = Ctor "Just" [add "fromJust" x]
    unpack x@(Ctor "Just" _) = Just $ get "fromJust" $ ctor "Just" x
    unpack _ = Nothing

instance (Packer a, Packer b) => Packer (Either a b) where
    pack (Left x) = Ctor "Left" [add "fromLeft" x]
    pack (Right x) = Ctor "Right" [add "fromRight" x]
    unpack x@(Ctor "Left" _) = Left $ get "fromLeft" $ ctor "Left" x
    unpack x@(Ctor "Right" _) = Right $ get "fromRight" $ ctor "Right" x
    unpack _ = Left $ unpack None

instance Packer Bool where
    pack True = Ctor "True" []
    pack _ = Ctor "False" []
    unpack (Ctor "True" _) = True
    unpack _ = False

instance Packer a => Packer (Group a) where
    pack Group{..} = Ctor "Group"
        [add "groupUnnamed" groupUnnamed
        ,add "groupHidden" groupHidden
        ,add "groupNamed" groupNamed]
    unpack x = let y = ctor "Group" x in Group
        {groupUnnamed = get "groupUnnamed" y
        ,groupHidden = get "groupHidden" y
        ,groupNamed = get "groupNamed" y}

instance Packer a => Packer (Mode a) where
    pack Mode{..} = Ctor "Mode"
        [add "modeGroupModes" modeGroupModes
        ,add "modeNames" modeNames
        ,add "modeHelp" modeHelp
        ,add "modeHelpSuffix" modeHelpSuffix
        ,add "modeArgs" modeArgs
        ,add "modeGroupFlags" modeGroupFlags
        ,add "modeValue" modeValue
        ,add "modeCheck" modeCheck
        ,add "modeReform" modeReform
        ,add "modeExpandAt" modeExpandAt]
    unpack x = let y = ctor "Mode" x in Mode
        {modeGroupModes = get "modeGroupModes" y
        ,modeNames = get "modeNames" y
        ,modeHelp = get "modeHelp" y
        ,modeHelpSuffix = get "modeHelpSuffix" y
        ,modeArgs = get "modeArgs" y
        ,modeGroupFlags = get "modeGroupFlags" y
        ,modeValue = get "modeValue" y
        ,modeCheck = get "modeCheck" y
        ,modeReform = get "modeReform" y
        ,modeExpandAt = get "modeExpandAt" y}

instance Packer a => Packer (Flag a) where
    pack Flag{..} = Ctor "Flag"
        [add "flagNames" flagNames
        ,add "flagInfo" flagInfo
        ,add "flagType" flagType
        ,add "flagHelp" flagHelp
        ,add "flagValue" flagValue]
    unpack x = let y = ctor "Flag" x in Flag
        {flagNames = get "flagNames" y
        ,flagInfo = get "flagInfo" y
        ,flagType = get "flagType" y
        ,flagHelp = get "flagHelp" y
        ,flagValue = get "flagValue" y}

instance Packer a => Packer (Arg a) where
    pack Arg{..} = Ctor "Arg"
        [add "argType" argType
        ,add "argRequire" argRequire
        ,add "argValue" argValue]
    unpack x = let y = ctor "Arg" x in Arg
        {argType = get "argType" y
        ,argRequire = get "argRequire" y
        ,argValue = get "argValue" y}

instance Packer FlagInfo where
    pack FlagReq = Ctor "FlagReq" []
    pack (FlagOpt x) = Ctor "FlagOpt" [add "fromFlagOpt" x]
    pack (FlagOptRare x) = Ctor "FlagOptRare" [add "fromFlagOpt" x]
    pack FlagNone = Ctor "FlagNone" []
    unpack x@(Ctor name _) = case name of
        "FlagReq" -> FlagReq
        "FlagOpt" -> FlagOpt $ get "fromFlagOpt" $ ctor name x
        "FlagOptRare" -> FlagOptRare $ get "fromFlagOpt" $ ctor name x
        "FlagNone" -> FlagNone
    unpack _ = FlagNone
