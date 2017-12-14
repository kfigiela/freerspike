{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}


module Main where

import           Control.Monad.Freer (Eff, Member, run, runM)

import           Domain
import           Languages
import           Lib

main :: IO ()
main = do
    servedWine <- runM $ runKitchen $ runCashDesk $ runBar restaurant
    print servedWine

restaurant :: (Member Kitchen r, Member Bar r, Member CashDesk r) => Eff r String
restaurant = do
    waitingTime1 <- orderPizza (Pizza "Margherita" Medium)
    waitingTime2 <- orderPizza (Pizza "Capricora" Small)
    wineName <- serveWine
    serveAppetizers $ waitingTime1 + waitingTime2
    complain $ "Does not match " ++ wineName
    billAmt <- makeBill
    payTheBill 12
    payTheBill $ billAmt - 12
    return wineName
