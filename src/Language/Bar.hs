module Language.Bar where

class Monad m => Bar m where
    serveWine :: m String
    serveAppetizers :: Int -> m ()
