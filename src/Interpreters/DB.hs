{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Interpreters.DB
  ( runDB
  ) where

import           Control.Exception (SomeException)
import           Eff               (Eff, Member, handleRelay)
import           Eff.Exc           (Exc)
import           Eff.Exc.Pure      (runError)
import           Eff.SafeIO        (SIO, safeIO)
import           Language.DB       (DB (..), TransactionException)

runDB :: (Member SIO r, Member (Exc SomeException) r) => Eff (DB ': r) a -> Eff r a
runDB = handleRelay pure (\k q -> interpret k >>= q)

interpret :: (Member SIO r, Member (Exc SomeException) r)  => DB x -> Eff r x
interpret BeginTransaction    = safeIO $ putStrLn "BEGIN TRANSACTION"
interpret RollbackTransaction = safeIO $ putStrLn "ROLLBACK"
interpret CommitTransaction   = safeIO $ putStrLn "COMMIT"
