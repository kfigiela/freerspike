{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
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

data CashDesk a where
    MakeBill    :: CashDesk Int
    DoSthStupid :: CashDesk () -- throws our exception
    PayTheBill  :: Int -> CashDesk (Maybe Int)

makeFreer ''CashDesk
