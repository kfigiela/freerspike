module Language.Kitchen where

import           Domain

class Monad m => Kitchen m where
    orderPizza :: Pizza -> m Int
    complain :: String -> m ()
