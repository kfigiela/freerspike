{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
module Language.Bar where

import           Eff.TH

-- just data types, no extra boilerplate

data Bar a where
    ServeWine :: Bar String
    ServeAppetizers :: Int -> Bar ()

-- generate smart constructors with TH
makeFreer ''Bar

-- or just do this manually

-- serveWine :: Member Bar effs => Eff effs String
-- serveWine = send ServeWine

-- serveAppetizers :: Member Bar effs => Int -> Eff effs ()
-- serveAppetizers = send . ServeAppetizers

