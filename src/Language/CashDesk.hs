{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Language.CashDesk where

import           Control.Exception (ArithException (..), SomeException, throwIO)
import           Eff
import           Eff.Exc
import           Eff.Exc.Pure
import           Eff.Internal      (Eff (..), Union, decomp, prj, qComp,
                                    tsingleton)
import           Eff.Region
import           Eff.TH
import           Language.DB       (TransactionException)

data CashDesk a where
    MakeBill    :: CashDesk Int
    DoSthStupid :: CashDesk (Either TransactionException ()) -- throws our exception
    PayTheBill  :: Int -> CashDesk (Maybe Int)


makeBill :: Member CashDesk effs => Eff effs Int
makeBill = send MakeBill

payTheBill :: Member CashDesk effs => Int -> Eff effs (Maybe Int)
payTheBill = send . PayTheBill

-- This one can throw exception: we require it to be run in Exc TransactionException environment.
-- Interpreter returns exception as Either(Left) and we rethrow it using Exc effect.
-- However, this is at cost of making this little boilerplate
doSthStupid :: (Member (Exc TransactionException) effs, Member CashDesk effs) => Eff effs ()
doSthStupid = send DoSthStupid >>= squelch
