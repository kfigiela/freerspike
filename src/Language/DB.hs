{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Language.DB
  ( transactionally
  , DB (..)
  , TransactionException (..)
  ) where

import           Eff
import           Eff.Exc
import           Eff.Exc.Pure
import           Eff.TH


newtype TransactionException = TransactionException String deriving (Show)

data DB a where
    BeginTransaction    :: DB () -- begin transaction
    CommitTransaction   :: DB () -- commit
    RollbackTransaction  :: DB () -- rollback

makeFreer ''DB

transactionally :: forall effs a. (Member DB effs) => Eff ((Exc TransactionException) ': effs) a -> Eff effs (Maybe a)
transactionally region = runError (doThings region) >>= process where
    doThings region = do
        beginTransaction
        region
    process (Left _) = do
        rollbackTransaction
        return Nothing
    process (Right res) = do
        commitTransaction
        return $ Just res
