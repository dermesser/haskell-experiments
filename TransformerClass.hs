module TransformerClass where

class MonadTrans m where
    lift :: (Monad n) => n a -> m n a


