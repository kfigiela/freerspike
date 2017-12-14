{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Interpreters where
import           Control.Monad.Freer
import           Domain
import           Language.Bar
import           Language.CashDesk
import           Language.Kitchen

runKitchen :: forall effs a. LastMember IO effs => Eff (Kitchen ': effs) a -> Eff effs a
runKitchen = interpretM $ \case
  OrderPizza pizza -> do
    print pizza
    return 42
  Complain complaint -> putStrLn complaint


runBar :: forall effs a. LastMember IO effs => Eff (Bar ': effs) a -> Eff effs a
runBar = interpretM (\case
    ServeWine -> do
        putStrLn "Serving some wine"
        return "Merlot"
    ServeAppetizers time -> putStrLn $ "Appetizers for waiting time: " ++ (show time))

runCashDesk :: forall effs a. LastMember IO effs => Eff (CashDesk ': effs) a -> Eff effs a
runCashDesk = interpretM (\case
    MakeBill -> do
        putStrLn "Printing bill"
        return 421
    PayTheBill amt -> do
        putStrLn $ "Paid " ++ (show amt)
        return Nothing)

