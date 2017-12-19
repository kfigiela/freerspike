{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Interpreters.DB
  ( runDB
  , TransactionState
  ) where

import           Control.Exception (SomeException)
import           Eff               (Eff, Member, handleRelay)
import           Eff.Exc           (Exc, throwError)
import           Eff.Exc.Pure      (catchError, runError)
import           Eff.SafeIO        (SIO, safeIO)
import           Eff.State.Pure
import           Language.DB       (DB (..), TransactionException,
                                    rollbackTransaction)

data TransactionHandle = TransactionHandle Int

type TransactionState = Maybe TransactionHandle

runDB :: (Member SIO effs, Member (Exc SomeException) effs) => Eff (DB ': (State TransactionState) ': effs) a -> Eff effs a
runDB eff = evalState (Nothing :: TransactionState) $ runDB' $ catchError eff handle
  where
    handle ::  (Member SIO effs, Member (State TransactionState) effs, Member DB effs, Member (Exc SomeException) effs) => SomeException -> Eff effs a
    handle exc = (safeIO $ print "DB exception") >> rollbackTransaction >> throwError exc
    runDB' :: (Member SIO effs, Member (State TransactionState) effs, Member (Exc SomeException) effs) => Eff (DB ': effs) a -> Eff effs a
    runDB' = handleRelay pure (\k q -> interpret k >>= q)

interpret :: (Member SIO effs, Member (State TransactionState) effs, Member (Exc SomeException) effs)  => DB x -> Eff effs x
interpret BeginTransaction    = do
    safeIO $ putStrLn "BEGIN TRANSACTION"
    put $ Just $ TransactionHandle 123
    return 123
interpret RollbackTransaction = do
  get >>= \case
    Just (TransactionHandle h) -> safeIO $ putStrLn $ "ROLLBACK " ++ show h
    Nothing -> safeIO $ putStrLn "NO TRANSACTION TO ROLLBACK"
  put (Nothing :: TransactionState)
interpret CommitTransaction  = do
  safeIO $ putStrLn "COMMIT"
  put (Nothing :: TransactionState)
