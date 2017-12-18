module Domain where

data Size = Small | Medium | Large    deriving (Show)
data Pizza = Pizza String Size deriving (Show)


