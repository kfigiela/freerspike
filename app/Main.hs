{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}


module Main where

import           Control.Exception     (SomeException)
import           Eff                   (Eff, Member, runM)
import           Eff.Exc               (Exc, throwError)
import           Eff.Region            (SafeForRegion)
import           Eff.SafeIO            (SIO, runSafeIO)

import           Domain
import           Interpreters.Bar      (runBar)
import           Interpreters.CashDesk (runCashDesk)
import           Interpreters.DB       (runDB)
import           Interpreters.Kitchen  (Oven, runKitchen)
import           Language.Bar          (Bar)
import qualified Language.Bar          as Bar
import           Language.CashDesk     (CashDesk)
import qualified Language.CashDesk     as CashDesk
import           Language.DB           (DB)
import qualified Language.DB           as DB
import           Language.Kitchen      (Kitchen)
import qualified Language.Kitchen      as Kitchen

instance SafeForRegion Oven '[DB, SIO, Exc SomeException]

main :: IO ()
main = do
    servedWine <- runSafeIO $ runDB $ runKitchen $ runBar $ runCashDesk restaurant
    print servedWine

-- reusable subscenario
payMyBill ::  (Member CashDesk r) => Eff r ()
payMyBill = do
    billAmt <- CashDesk.makeBill
    CashDesk.payTheBill 12
    CashDesk.payTheBill $ billAmt - 12
    return ()

-- example scenario
restaurant :: (Member DB r, Member Kitchen r, Member Bar r, Member CashDesk r) => Eff r (Maybe String)
restaurant = do
    w <- DB.transactionally $ do
        msg <- CashDesk.doSthSmart
        Kitchen.complain $ "Message from smart waiter " ++ msg
        waitingTime1 <- Kitchen.makePizza (Pizza "Margherita" Medium)
        msg' <- CashDesk.doSthStupid
        Kitchen.complain $ "Message from stupid waiter " ++ msg'
        waitingTime2 <- Kitchen.makePizza (Pizza "Capricora" Small)
        wineName <- Bar.serveWine
        Bar.serveAppetizers $ waitingTime1 + waitingTime2
        Kitchen.complain $ "Food does not match " ++ wineName
        -- payMyBill
        return wineName
    Kitchen.complain $ "This still works"
    return w
