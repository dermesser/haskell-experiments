module Transformers where


import Data.Monoid
import Control.Monad (liftM)
--import Data.Functor.Identity
--import Control.Monad.Identity (Identity)

-- Transformer class

class MonadTrans m where
    lift :: (Monad n) => n a -> m n a

-- Maybe Transformer
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    m >>= f = MaybeT $ do
                        m' <- runMaybeT m
                        case m' of
                            Nothing -> return Nothing
                            Just x -> runMaybeT $ f x

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just


-- Reader Transformer

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad m => Monad (ReaderT r m) where
    return x = ReaderT $ \_ -> return x
    m >>= f = ReaderT $ \x -> do
                        m' <- runReaderT m x
                        runReaderT (f m') x

instance MonadTrans (ReaderT r) where
    lift x = ReaderT $ \_ -> x

ask :: Monad m => ReaderT r m r
ask = ReaderT return

-- State Transformer

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Monad m => Monad (StateT s m) where
    return x = StateT $ \s -> return (x,s)
    m >>= f = StateT $ \s -> do
                                (x,s') <- runStateT m s
                                runStateT (f x) s'

instance MonadTrans (StateT s) where
    lift x = StateT $ \s -> x >>= \x' -> return (x',s)

-- Writer Transformer

newtype WriterT w m a = WriterT { runWriterT :: m (a,w) }

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return x = WriterT $ return (x,mempty)
    m >>= f = WriterT $ do
                        (a,w) <- runWriterT m
                        (a',w') <- runWriterT (f a)
                        return (a',w `mappend` w')

instance Monoid w => MonadTrans (WriterT w) where
    lift x = WriterT $ x >>= \x' -> return (x',mempty)

-- Identity Transformer

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
    return = Identity
    m >>= f = f (runIdentity m)

--
newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance Monad m => Monad (IdentityT m) where
    return = IdentityT . return
    m >>= f = IdentityT $ do
                            m' <- runIdentityT m
                            runIdentityT $ f m'

instance MonadTrans IdentityT where
    lift x = IdentityT x


-- ErrorT

newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

instance Monad m => Monad (ErrorT e m) where
    return x = ErrorT (return $ Right x)
    m >>= f = ErrorT $ do
                i <- runErrorT m
                case i of
                    Left x -> return (Left x)
                    Right y -> runErrorT $ f y

instance MonadTrans (ErrorT e) where
    lift x = ErrorT $ x >>= \x' -> return (Right x')
----

type Reader' r a = ReaderT r Identity a

reader :: (r -> a) -> Reader' r a
reader f = ReaderT (Identity . f)

runReader' :: Reader' r a -> r -> a
runReader' r = runIdentity . (runReaderT r)

type Maybe' a = MaybeT Identity a

just :: a -> Maybe' a
just = MaybeT . Identity . Just

nothing :: Maybe' a
nothing = MaybeT $ Identity Nothing

type State' s a = StateT s Identity a

state :: (s -> (a,s)) -> State' s a
state f = StateT (Identity . f)

runState :: State' s a -> s -> (a,s)
runState st = runIdentity . runStateT st

type Writer' w a = WriterT w Identity a

writer :: Monoid w => (a,w) -> Writer' w a
writer t = WriterT (Identity t)

runWriter :: Monoid w => Writer' w a -> (a,w)
runWriter = runIdentity . runWriterT

-----

type MyT = IdentityT (ReaderT Int (WriterT String (StateT [Int] Identity))) Int

pop :: State' [Int] Int
pop = state $ \(s:ss) -> (s,ss)

push :: Int -> State' [Int] ()
push i = state $ \s -> ((),i:s)

pushs :: [Int] -> State' [Int] ()
pushs is = state $ \s -> ((),is++s)

testf :: MyT
testf = do
        lift . lift . lift $ pushs [1,2,3,4]
        lift . lift . WriterT $ return (0,"pushed [1,2,3,4]")
        lift . lift . lift $ pop
        lift . lift . lift . lift $ Identity (0,"pop")
        lift . lift . lift $ pushs [5,6,7,8]
        lift . lift . WriterT $ return (0,"pushed [5,6,7,8]")
        lift . lift . lift $ pop
        lift . lift . WriterT $ return (0,"pop")
        lift . lift . lift $ pop
        x <- lift ask
        lift . lift . lift $ push x
        lift . lift . lift $ pop
--

type MyT' = StateT [Int] (ReaderT Int (Identity)) Int

pop' = StateT $ \(e:es) -> return (e,es)
push' x = StateT $ \s -> ReaderT $ \_ -> ((),x:s)

pushdef = StateT $ \s -> ReaderT $ \r -> Identity ((),r:s)

testf2 :: MyT'
testf2 = do
    push' 4
    push' 4
    pushdef
    pop'
    pushdef
    pushdef
    pop'

