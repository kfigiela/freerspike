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
runKitchen = handleRelay pure (\k q -> interpret k >>= q) where
    interpret :: (Member SIO r, Member (Exc SomeException) r)  => Kitchen x -> Eff r x
    interpret (OrderPizza pizza) = safeIO (do
          print pizza
          return 12)

    interpret (Complain complaint) = safeIO (print complaint)

runBar :: (Member SIO r, Member (Exc SomeException) r) => Eff (Bar ': r) a -> Eff r a
runBar = handleRelay pure (\k q -> interpret k >>= q) where
    interpret :: (Member SIO r, Member (Exc SomeException) r)  => Bar x -> Eff r x
    interpret (ServeWine) = safeIO $ do
                putStrLn "Serving some wine"
                return "Merlot"
    interpret (ServeAppetizers time) = safeIO $ putStrLn $ "Appetizers for waiting time: " ++ show time


runCashDesk :: (Member SIO r, Member (Exc SomeException) r) => Eff (CashDesk ': r) a -> Eff r a
runCashDesk = handleRelay pure (\k q -> interpret k >>= q) where
    interpret :: (Member SIO r, Member (Exc SomeException) r)  => CashDesk x -> Eff r x
    interpret (MakeBill) = safeIO $ do
        putStrLn "Printing bill"
        return 421
    interpret (PayTheBill amt) = safeIO $ do
      putStrLn $ "Paid " ++ show amt
      return Nothing
