{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Languages where
import           Control.Monad.Freer
import           Domain


data Kitchen a where
    OrderPizza :: Pizza -> Kitchen Int
    Complain :: String -> Kitchen ()

orderPizza :: Member Kitchen effs => Pizza -> Eff effs Int
orderPizza = send . OrderPizza

complain :: Member Kitchen effs => String -> Eff effs ()
complain = send . Complain

data Bar a where
    ServeWine :: Bar String
    ServeAppetizers :: Int -> Bar ()

serveWine :: Member Bar effs => Eff effs String
serveWine = send ServeWine

serveAppetizers :: Member Bar effs => Int -> Eff effs ()
serveAppetizers = send . ServeAppetizers

data CashDesk a where
    MakeBill :: CashDesk Int
    PayTheBill :: Int -> CashDesk (Maybe Int)

makeBill :: Member CashDesk effs => Eff effs Int
makeBill = send MakeBill

payTheBill :: Member CashDesk effs => Int -> Eff effs (Maybe Int)
payTheBill = send . PayTheBill




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

