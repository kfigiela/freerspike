{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Language.DB where

import           Control.Exception (ArithException (..), SomeException, throwIO)
import           Eff
import           Eff.Exc
import           Eff.Exc.Pure
import           Eff.Internal      (Eff (..), Union, decomp, prj, qComp,
                                    tsingleton)
import           Eff.Region
import           Eff.TH


data TransactionException = TransactionException String deriving (Show)

data DB a where
    BeginTransaction    :: DB () -- begin transaction
    CommitTransaction   :: DB () -- commit
    RollbackTransaction  :: DB () -- rollback

makeFreer ''DB

-- catchTransactionExcs :: forall (b :: *) effs a
--                  . Member (Exc TransactionException) effs
--                 => Eff effs ()
--                 -> (Union effs b -> Eff effs a)
--                 -> Union effs b
--                 -> Eff effs a
-- catchTransactionExcs releaseAll ignore u =
--   case prj u of
--     Just (Exc e) -> releaseAll >> throwError (e :: TransactionException)
--     Nothing      -> ignore u



-- -- datatype that represents a resource, could be newtype on file handle
-- data Transaction = Transaction deriving (Eq, Show)

-- instance SafeForRegion Transaction '[DB, Exc TransactionException]
-- instance SafeForRegion Transaction '[Exc TransactionException, DB]
-- instance SafeForRegion Transaction '[DB]

-- -- datatype that carries data to resource constructor (see allocateOven )
-- type TransactionData = ()

-- -- it says that OvenId is resouce counstructor type for Oven resource
-- type instance ResourceCtor Transaction = TransactionData

-- here we bind oven region with alloc/release functions
-- transactionRegion :: forall effs a. (SafeForRegion Transaction effs, Member DB effs, Member (Exc TransactionException) effs) => Region Transaction effs a -> Eff effs a
-- transactionRegion = handleRegionRelay allocate release catchCashDeskExcs
--   where
--     allocate _ = do
--         send OpenBill
--         return Transaction
--     release _ = return ()

-- this wraps acquire with types set for Oven resource
-- beginTransactionTrans :: forall r s. (s ~ Ancestor 0 r, Member (RegionEff Transaction s) r) =>  Eff r (Resource Transaction s)
-- beginTransactionTrans = acquire @Transaction ()

-- -- wrapper for catchError
-- transactionally :: forall effs a. (SafeForRegion Transaction ((Exc TransactionException) ': effs), Member DB effs) => Region Transaction ((Exc TransactionException) ': effs) a -> Eff effs (Maybe a)
-- transactionally region = runError (doThings region) >>= process where
--     doThings :: forall effs2. (SafeForRegion Transaction effs2, Member DB effs2, Member (Exc TransactionException) effs2) => Region Transaction effs2 a -> Eff effs2 a
--     doThings region = do
--         res <- handleRegionRelay allocate release catchTransactionExcs $ do
--             acquire @Transaction ()
--             region
--         commitTransaction
--         return res
--     process (Left _) = do
--         rollbackTransaction
--         return Nothing
--     process (Right res) = do
--         return $ Just res
--     allocate _ = do
--         beginTransaction
--         return Transaction
--     release _ = return ()
-- wrapper for catchError
transactionally :: forall effs a. (Member DB effs) => Eff ((Exc TransactionException) ': effs) a -> Eff effs (Maybe a)
transactionally region = runError (doThings region) >>= process where
    -- doThings :: forall a. (Member DB effs) => Eff ((Exc TransactionException) ': effs) a -> Eff effs a
    doThings region = do
        beginTransaction
        region
    process (Left _) = do
        rollbackTransaction
        return Nothing
    process (Right res) = do
        commitTransaction
        return $ Just res


-- -- extra wrapper to simulate Ruby's File.open filename do |file| ...things end syntax
-- withinTransaction :: forall effs s b. (s ~ Ancestor 0 effs, Member DB effs, Member (RegionEff Transaction s) effs) => Eff effs b -> Eff effs b
-- withinTransaction region = openBill >> region
