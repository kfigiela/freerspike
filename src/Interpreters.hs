{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}


module Interpreters where
import           Control.Exception (SomeException)
import           Domain
import           Eff
import           Eff.Exc
import           Eff.SafeIO        (SIO, safeIO)
import           Language.Bar
import           Language.CashDesk
import           Language.Kitchen


runKitchen :: (Member SIO r, Member (Exc SomeException) r) => Eff (Kitchen ': r) a -> Eff r a
runKitchen = handleRelay pure interpret where
    interpret :: (Member SIO r, Member (Exc SomeException) r)  => Kitchen x -> Arr r x a -> Eff r a
    interpret (OrderPizza pizza) q = q =<< safeIO (do
        print pizza
        return 12)

    interpret (Complain complaint) q = q =<< safeIO (print complaint)

runBar :: (Member SIO r, Member (Exc SomeException) r) => Eff (Bar ': r) a -> Eff r a
runBar = handleRelay pure interpret where
    interpret :: (Member SIO r, Member (Exc SomeException) r)  => Bar x -> Arr r x a -> Eff r a
    interpret (ServeWine) q = q =<< safeIO (do
                putStrLn "Serving some wine"
                return "Merlot")
    interpret (ServeAppetizers time) q = q =<< safeIO (putStrLn $ "Appetizers for waiting time: " ++ show time)


runCashDesk :: (Member SIO r, Member (Exc SomeException) r) => Eff (CashDesk ': r) a -> Eff r a
runCashDesk = handleRelay pure interpret where
    interpret :: (Member SIO r, Member (Exc SomeException) r)  => CashDesk x -> Arr r x a -> Eff r a
    interpret (MakeBill) q = q =<< safeIO (do
        putStrLn "Printing bill"
        return 421)
    interpret (PayTheBill amt) q = q =<< safeIO (do
      putStrLn $ "Paid " ++ show amt
      return Nothing)
