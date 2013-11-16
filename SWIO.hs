import Data.Monoid
import System.IO

main = undefined

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

