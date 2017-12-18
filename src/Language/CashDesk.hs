{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}

module Language.CashDesk where

import           Eff.TH

data CashDesk a where
    MakeBill :: CashDesk Int
    PayTheBill :: Int -> CashDesk (Maybe Int)

makeFreer ''CashDesk
