{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}


module Scenarios where

import           Control.Exception (SomeException)
import           Eff               (Eff, Member, runM)
import           Eff.Exc           (Exc, throwError)
import           Eff.Region        (SafeForRegion)
import           Eff.SafeIO        (SIO, runSafeIO)
import           Eff.State         (State)

import           Domain

import           Language.Bar      (Bar)
import qualified Language.Bar      as Bar
import           Language.CashDesk (CashDesk)
import qualified Language.CashDesk as CashDesk
import           Language.DB       (DB)
import qualified Language.DB       as DB
import           Language.Kitchen  (Kitchen)
import qualified Language.Kitchen  as Kitchen


-- #####################################
-- This scenario is to show how multiple DSLs can be composed
-- #####################################

-- reusable subscenario
payMyBill ::  (Member CashDesk r) => Eff r ()
payMyBill = do
    billAmt <- CashDesk.makeBill
    CashDesk.payTheBill 12
    CashDesk.payTheBill $ billAmt - 12
    return ()

scenario1 :: (Member DB r, Member Kitchen r, Member Bar r, Member CashDesk r) => Eff r (Maybe String)
scenario1 = do
    w <- DB.transactionally $ do
        waitingTime1 <- Kitchen.makePizza (Pizza "Margherita" Medium)
        waitingTime2 <- Kitchen.makePizza (Pizza "Capricora" Small)
        wineName <- Bar.serveWine
        Bar.serveAppetizers $ waitingTime1 + waitingTime2
        Kitchen.complain $ "Food does not match " ++ wineName
        payMyBill
        return wineName
    Kitchen.complain  "This still works"
    return w

-- #####################################
-- These scenarios are to show how transactions can be handled
-- #####################################

dbTransactions1 :: (Member DB r, Member Kitchen r, Member Bar r, Member CashDesk r) => Eff r (Maybe String)
dbTransactions1 = do
    result <- DB.transactionally $ do
        msg <- CashDesk.doSthSmart -- this HAS to run within transaction
        Kitchen.complain msg
        return msg
    Kitchen.complain $ "This runs regardless of transaction status, result = " ++ show result
    return result

dbTransactions2 :: (Member DB r, Member Kitchen r, Member Bar r, Member CashDesk r) => Eff r (Maybe String)
dbTransactions2 = do
    result <- DB.transactionally $ do
        msg <- CashDesk.doSthStupid -- this HAS to run within transaction
        Kitchen.complain msg
        return msg
    Kitchen.complain $ "This runs regardless of transaction status, result = " ++ show result
    return result

dbTransactions3 :: (Member DB r, Member Kitchen r, Member Bar r, Member CashDesk r) => Eff r (Maybe String)
dbTransactions3 = do
    result <- DB.transactionally $ do
        let a = 1
            b = 0
            c = 1 `div` 0 -- this will crash when it's evaluated (when printing from SafeIO)
        return $ show $ c
    Kitchen.complain $ "This runs regardless of transaction status, result = " ++ show result
    return result

dbTransactions4 :: (Member DB r, Member Kitchen r, Member Bar r, Member CashDesk r) => Eff r (Maybe String)
dbTransactions4 = do
    result <- DB.transactionally $ do
        let a = 1
            b = 0
            !c = 1 `div` 0 -- this will crash here, outside of SafeIO and we end up with unhandled exception
        return $ show c
    Kitchen.complain $ "This runs regardless of transaction status, result = " ++ show result
    return result

allScenarios :: (Member DB r, Member Kitchen r, Member Bar r, Member CashDesk r) => [Eff r (Maybe String)]
allScenarios =
    [ scenario1
    , dbTransactions1
    , dbTransactions2
    , dbTransactions3
    , dbTransactions4
    ]
