{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Interpreters.CashDesk
  ( runCashDesk
  ) where

import           Control.Exception (ArithException (..), SomeException, throwIO)
import           Eff               (Eff, Member, handleRelay)
import           Eff.Exc           (Exc, throwError)
import           Eff.Exc.Pure      (runError)
import           Eff.SafeIO        (SIO, safeIO)
import           Language.CashDesk (CashDesk (..))
import           Language.DB

runCashDesk :: (Member SIO effs, Member (Exc SomeException) effs) => Eff (CashDesk ': effs) a -> Eff effs a
runCashDesk = handleRelay pure (\k q -> interpret k >>= q)

interpret :: (Member SIO effs, Member (Exc SomeException) effs)  => CashDesk x -> Eff effs x
interpret DoSthStupid' = return $ Left $ TransactionException "I'm stupid"
interpret DoSthSmart'  = return $ Right "I'm smart"
interpret MakeBill = safeIO $ do
    putStrLn "Printing bill"
    return 421
interpret (PayTheBill amt) = safeIO $ do
    putStrLn $ "Paid " ++ show amt
    return Nothing
