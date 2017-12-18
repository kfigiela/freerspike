{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Interpreters.CashDesk
  ( runCashDesk
  ) where

import           Control.Exception (SomeException)
import           Eff               (Eff, Member, handleRelay)
import           Eff.Exc           (Exc)
import           Eff.SafeIO        (SIO, safeIO)
import           Language.CashDesk (CashDesk (..))

runCashDesk :: (Member SIO r, Member (Exc SomeException) r) => Eff (CashDesk ': r) a -> Eff r a
runCashDesk = handleRelay pure (\k q -> interpret k >>= q)

interpret :: (Member SIO r, Member (Exc SomeException) r)  => CashDesk x -> Eff r x
interpret MakeBill = safeIO $ do
    putStrLn "Printing bill"
    return 421
interpret (PayTheBill amt) = safeIO $ do
    putStrLn $ "Paid " ++ show amt
    return Nothing
