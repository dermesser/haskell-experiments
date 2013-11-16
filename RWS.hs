module RWS where

{-
    This is my implementation of the RWS and RWST monads. RWS doesn't use transformers
    but is a standalone combination monad with the functionality of Reader, Writer and State.
-}


import TransformerClass

import Data.Functor.Identity
import Control.Monad.Identity (Identity,runIdentity)
import Control.Monad (Monad)
import Data.Monoid

newtype RWS s r w a = RWS { runRWS :: s -> r -> (a,w,s) }

instance Monoid w => Monad (RWS s r w) where
    return x = RWS $ \s _ -> (x,mempty,s)
    -- orst = "Original State"
    m >>= f = RWS $ \orst r -> let (a,w,s) = runRWS m orst r;
                                   rws' = f a;
                                   (a',w',s') = runRWS rws' s r
                         in (a',w `mappend` w',s')

--

newtype RWST s r w m a = RWST { runRWST :: s -> r -> m (a,w,s) }

instance (Monoid w, Monad m) => Monad (RWST s r w m) where
    return x = RWST $ \s _ -> return (x,mempty,s)
    m >>= f = RWST $ \olds r -> do
                                (a,w,s) <- runRWST m olds r
                                (a',w',s') <- runRWST (f a) s r
                                return (a',w `mappend` w', s')

instance Monoid w => MonadTrans (RWST s r w) where
    lift x = RWST $ \s _r -> x >>= \x' -> return (x',mempty,s)

--

type RWS' s r w a = RWST s r w Identity a

rws' :: (s -> r -> (a,w,s)) -> RWS' s r w a
rws' f = RWST $ \s r -> Identity (f s r)

runRWS' :: RWS' s r w a -> s -> r -> (a,w,s)
runRWS' rws s r = runIdentity (runRWST rws s r)

--

push :: Int -> RWS' [Int] Int String ()
push x = rws' $ \s _r -> ((),"Pushed " ++ show x ++ "\n",x:s)

pushDefault :: RWS' [Int] Int String ()
pushDefault = rws' $ \s r -> ((),"Pushed default: " ++ show r ++ "\n",r:s)

testf = do
    push 7
    push 8
    pushDefault
