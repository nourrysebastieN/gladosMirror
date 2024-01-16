{-# LANGUAGE DeriveDataTypeable #-}

module System.Console.Hawk.Verbosity(
    Verbosity(..), setVerbosity, getVerbosity,
    isNormal, isLoud,
    whenNormal, whenLoud
    ) where

import Control.Monad
import Data.Data
import Data.IORef
import System.IO.Unsafe

data Verbosity
    = Quiet
    | Normal
    | Loud
      deriving (Eq,Ord,Bounded,Enum,Show,Read,Data,Typeable)

{-# NOINLINE ref #-}
ref :: IORef Verbosity
ref = unsafePerformIO $ newIORef Normal

setVerbosity :: Verbosity -> IO ()
setVerbosity = writeIORef ref

getVerbosity :: IO Verbosity
getVerbosity = readIORef ref

isNormal :: IO Bool
isNormal = fmap (>=Normal) getVerbosity

isLoud :: IO Bool
isLoud = fmap (>=Loud) getVerbosity

whenNormal :: IO () -> IO ()
whenNormal act = do
    b <- isNormal
    when b act

whenLoud :: IO () -> IO ()
whenLoud act = do
    b <- isLoud
    when b act
