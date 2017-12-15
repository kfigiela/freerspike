{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}


module Main where

import           Eff               (Eff, Member, runM)
import           Eff.SafeIO        (runSafeIO)

import           Domain
import           Interpreters
import           Language.Bar      (Bar)
import qualified Language.Bar      as Bar
import           Language.CashDesk (CashDesk)
import qualified Language.CashDesk as CashDesk
import           Language.Kitchen  (Kitchen)
import qualified Language.Kitchen  as Kitchen

main :: IO ()
main = do
    servedWine <- runSafeIO $ runKitchen  $ runBar $ runCashDesk restaurant
    print servedWine

-- reusable subscenario
payMyBill ::  (Member CashDesk r) => Eff r ()
payMyBill = do
    billAmt <- CashDesk.makeBill
    CashDesk.payTheBill 12
    CashDesk.payTheBill $ billAmt - 12
    return ()

-- example scenario
restaurant :: (Member Kitchen r, Member Bar r, Member CashDesk r) => Eff r String
restaurant = do
    waitingTime1 <- Kitchen.orderPizza (Pizza "Margherita" Medium)
    waitingTime2 <- Kitchen.orderPizza (Pizza "Capricora" Small)
    wineName <- Bar.serveWine
    Bar.serveAppetizers $ waitingTime1 + waitingTime2
    Kitchen.complain $ "Food does not match " ++ wineName
    payMyBill
    return wineName
