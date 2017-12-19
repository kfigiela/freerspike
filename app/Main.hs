{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}


module Main where

import           Control.Exception     (SomeException, try)
import           Eff                   (Eff, Member, runM)
import           Eff.Exc               (Exc, throwError)
import           Eff.Region            (SafeForRegion)
import           Eff.SafeIO            (SIO, runSafeIO)
import           Eff.State             (State)

import           Domain
import           Interpreters.Bar      (runBar)
import           Interpreters.CashDesk (runCashDesk)
import           Interpreters.DB       (TransactionState, runDB)
import           Interpreters.Kitchen  (Oven, runKitchen)
import           Language.Bar          (Bar)
import qualified Language.Bar          as Bar
import           Language.CashDesk     (CashDesk)
import qualified Language.CashDesk     as CashDesk
import           Language.DB           (DB)
import qualified Language.DB           as DB
import           Language.Kitchen      (Kitchen)
import qualified Language.Kitchen      as Kitchen
import           Scenarios             (allScenarios)

instance SafeForRegion Oven '[DB, State TransactionState, SIO, Exc SomeException]

main :: IO ()
main = do
    mapM_ printScenario allScenarios
    putStrLn "\n\n ====== FINISHED ======"

printScenario scenario = do
    result <- runScenario scenario
    let msg = case result of
            Left error -> "Unhandled exception: " ++ show error
            Right Nothing -> "Handled exception happened, runtime aborted (rollback should happen)"
            Right (Just result) -> "Success: " ++ show result
    putStrLn $ "FINISHED: " ++ msg ++ "\n\n===========\n\n"
runScenario :: Eff '[CashDesk, Bar, Kitchen, DB, State TransactionState, SIO, Exc SomeException] w -> IO (Either SomeException w)
runScenario = try . runSafeIO . runDB . runKitchen . runBar . runCashDesk
