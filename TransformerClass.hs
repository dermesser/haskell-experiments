module TransformerClass where

{-
    This file contains the type class for monad transformers, implementing lift.
-}

class MonadTrans m where
    lift :: (Monad n) => n a -> m n a


