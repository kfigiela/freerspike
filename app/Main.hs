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
import           Interpreters.DB       (runDB, runDBErr)
import           Interpreters.Kitchen  (Oven, runKitchen)
import           Language.Bar          (Bar)
import qualified Language.Bar          as Bar
import           Language.CashDesk     (CashDesk)
import qualified Language.CashDesk     as CashDesk
import           Language.DB           (DB)
import qualified Language.DB           as DB
import           Language.Kitchen      (Kitchen)
import qualified Language.Kitchen      as Kitchen

-- instance SafeForRegion DB.Transaction '[DB, SIO, Exc SomeException]
-- instance SafeForRegion Oven '[Exc DB.TransactionException, DB, SIO, Exc SomeException]
instance SafeForRegion Oven '[Exc DB.TransactionException, DB, SIO, Exc SomeException]
-- instance SafeForRegion DB.Transaction '[CashDesk, Bar, Kitchen, DB, SIO, Exc SomeException]
-- instance SafeForRegion DB.Transaction '[Exc DB.TransactionException, CashDesk, Bar, Kitchen, DB, SIO, Exc SomeException]
-- instance SafeForRegion DB.Transaction '[Exc DB.TransactionException, CashDesk, Bar, Kitchen, Exc DB.TransactionException, DB, SIO, Exc SomeException]
-- -- instance SafeForRegion DB.Transaction '[CashDesk, Bar, Kitchen, DB, SIO, Exc SomeException]


main :: IO ()
main = do
    servedWine <- runSafeIO $ runDB $ runDBErr $ runKitchen $ runBar $ runCashDesk restaurant
    print servedWine

-- reusable subscenario
payMyBill ::  (Member CashDesk r) => Eff r ()
payMyBill = do
    -- CashDesk.transactionally $ CashDesk.withinTransaction $ do
    billAmt <- CashDesk.makeBill
    CashDesk.payTheBill 12
    CashDesk.payTheBill $ billAmt - 12
    return ()

-- example scenario
restaurant :: (Member DB r, Member Kitchen r, Member Bar r, Member CashDesk r) => Eff r (Maybe String)
restaurant = do
    w <- DB.transactionally $ do
        waitingTime1 <- Kitchen.makePizza (Pizza "Margherita" Medium)
        CashDesk.doSthStupid
        throwError $ DB.TransactionException "I'm even more stupid"
        waitingTime2 <- Kitchen.makePizza (Pizza "Capricora" Small)
        wineName <- Bar.serveWine
        Bar.serveAppetizers $ waitingTime1 + waitingTime2
        Kitchen.complain $ "Food does not match " ++ wineName
        -- payMyBill
        return wineName
    Kitchen.complain $ "This still works"
    return w
