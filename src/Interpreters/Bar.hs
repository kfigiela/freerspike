{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Interpreters.Bar
  ( runBar
  ) where

import           Control.Exception (SomeException)
import           Eff               (Eff, Member, handleRelay)
import           Eff.Exc           (Exc)
import           Eff.SafeIO        (SIO, safeIO)
import           Language.Bar      (Bar (..))

runBar :: (Member SIO r, Member (Exc SomeException) r) => Eff (Bar ': r) a -> Eff r a
runBar = handleRelay pure (\k q -> interpret k >>= q)

interpret :: (Member SIO r, Member (Exc SomeException) r)  => Bar x -> Eff r x
interpret ServeWine = safeIO $ do
            putStrLn "Serving some wine"
            return "Merlot"
interpret (ServeAppetizers time) = safeIO $ putStrLn $ "Appetizers for waiting time: " ++ show time
