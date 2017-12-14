{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Language.Kitchen where
import           Control.Monad.Freer
import           Domain


data Kitchen a where
    OrderPizza :: Pizza -> Kitchen Int
    Complain :: String -> Kitchen ()

orderPizza :: Member Kitchen effs => Pizza -> Eff effs Int
orderPizza = send . OrderPizza

complain :: Member Kitchen effs => String -> Eff effs ()
complain = send . Complain
