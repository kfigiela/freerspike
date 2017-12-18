{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}

module Language.Kitchen where

import           Domain
import           Eff.TH


data Kitchen a where
    MakePizza :: Pizza -> Kitchen Int
    Complain :: String -> Kitchen ()

makeFreer ''Kitchen
