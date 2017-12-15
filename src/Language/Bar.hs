{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Language.Bar where
import           Domain
import           Eff

-- just data types, no extra boilerplate

data Bar a where
    ServeWine :: Bar String
    ServeAppetizers :: Int -> Bar ()

serveWine :: Member Bar effs => Eff effs String
serveWine = send ServeWine

serveAppetizers :: Member Bar effs => Int -> Eff effs ()
serveAppetizers = send . ServeAppetizers
