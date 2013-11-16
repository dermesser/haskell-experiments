module SWIO where

{-
    SWIO combines the State, Writer and IO monads. Is an experiment for
    my Brainfuch interpreter which just needs such a type (so far realized
    using mtl monad transformers: WriterT [StackCode] (StateT Stack IO)
-}

import Data.Monoid
import System.IO

newtype WSIO w s a = WSIO {runWSIO :: s -> IO (a,w,s) }

instance Monoid w => Monad (WSIO w s) where
    return x = WSIO $ \s -> return (x,mempty,s)
    m >>= f = WSIO $ \s -> do
                        (a,w,s) <- runWSIO m s
                        (a',w',s') <- runWSIO (f a) s
                        return (a',w `mappend` w', s')

f1 = WSIO $ \s -> putStrLn "Addition!!" >> return (s,"Add ",s+1)
f2 = WSIO $ \s -> putStrLn "Subtraction!!" >> return (s,"Subtract ",s-1)

testf = do
    f1
    f2

