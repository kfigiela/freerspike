module Language.CashDesk where

class Monad m => CashDesk m where
    makeBill :: m Int
    payTheBill :: Int -> m (Maybe Int)
