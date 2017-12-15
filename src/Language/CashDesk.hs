{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Language.CashDesk where
import           Domain
import           Eff

data CashDesk a where
    MakeBill :: CashDesk Int
    PayTheBill :: Int -> CashDesk (Maybe Int)

makeBill :: Member CashDesk effs => Eff effs Int
makeBill = send MakeBill

payTheBill :: Member CashDesk effs => Int -> Eff effs (Maybe Int)
payTheBill = send . PayTheBill

