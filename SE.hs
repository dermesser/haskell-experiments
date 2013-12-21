module SE where

import Control.Monad
import TransformerClass

-- Standard monad
newtype SE s e a = SE { runSE :: s -> Either e (a,s) }

instance Monad (SE s e) where
    return x = SE $ \s -> Right (x,s)
    m >>= f = SE $ \s ->
                case runSE m s of
                    Left e -> Left e
                    Right (a,s') -> runSE (f a) s'

-- Transformer
newtype SET s e m a = SET { runSET :: s -> m (Either e (a,s)) }

instance Monad m => Monad (SET s e m) where
    return x = SET $ \s -> return (Right (x,s))
    m >>= f = SET $ \s ->
            do
                r <- runSET m s
                case r of
                    Left e -> return $ Left e
                    Right (a,s') -> runSET (f a) s'

instance MonadTrans (SET s e) where
    lift x = SET $ \s -> x >>= \x' -> return (Right (x',s))

