{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Interpreters.Kitchen
  ( runKitchen, Oven
  ) where

import           Control.Exception (ArithException (..), SomeException, throwIO)
import           Eff
import           Eff.Exc           (Exc)
import           Eff.Exc.Pure      (catchError)
import           Eff.Region        (Ancestor, Region, RegionEff, Resource,
                                    ResourceCtor, SafeForRegion, acquire,
                                    catchSafeIOExcs, handleRegionRelay,
                                    unsafeWithResource)
import           Eff.SafeIO        (SIO, safeIO)
import           Language.Kitchen

newtype Temperature = Temperature Int deriving (Show, Eq)

-- datatype that represents a resource, could be newtype on file handle
data Oven = Oven Temperature deriving (Eq, Show)

instance SafeForRegion Oven '[SIO, Exc SomeException]

-- datatype that carries data to resource constructor (see allocateOven )
newtype OvenId = OvenId Int deriving (Show)

-- it says that OvenId is resouce counstructor type for Oven resource
type instance ResourceCtor Oven = OvenId

-- here we bind oven region with alloc/release functions
ovenRegion :: forall effs a. (SafeForRegion Oven effs, Member SIO effs, Member (Exc SomeException) effs) => Region Oven effs a -> Eff effs a
ovenRegion = handleRegionRelay allocateOven releaseOven catchSafeIOExcs
  where
    -- here we can allocate our resource that is handled by region
    -- note that we may allocate more than one resources in single region - they will be all released
    allocateOven (OvenId n) = do
      safeIO $ putStrLn $ "heating up oven no " ++ show n
      return (Oven (Temperature 200))
    -- here we release allocated resource, this will always run even if region is terminated by exception
    releaseOven (Oven _) = safeIO $ putStrLn "Turned off oven"

-- this wraps acquire with types set for Oven resource
startOven :: forall r s. (s ~ Ancestor 0 r, Member (RegionEff Oven s) r) => OvenId -> Eff r (Resource Oven s)
startOven = acquire @Oven

-- wrapper for catchError
safelyBake :: forall effs b. (SafeForRegion Oven effs, Member SIO effs, Member (Exc SomeException) effs) => Region Oven effs () -> Eff effs ()
safelyBake region = catchError (ovenRegion region) handleOvenFire

-- extra wrapper to simulate Ruby's File.open filename do |file| ...things end syntax
withOven :: forall effs s b. ( s ~ Ancestor 0 effs, Member (RegionEff Oven s) effs) => OvenId -> (Resource Oven s -> Eff effs b) -> Eff effs b
withOven ovenId region = startOven ovenId >>= region

-- Our error handler for catchError, we return () but
handleOvenFire :: (Member SIO effs, Member (Exc SomeException) effs) => SomeException -> Eff effs ()
handleOvenFire _err = safeIO $ print "Oven is on fire! Abort mission!"

-- main of our interpreter - possibly this could be generalized and put to library
runKitchen :: (SafeForRegion Oven r, Member SIO r, Member (Exc SomeException) r) => Eff (Kitchen ': r) a -> Eff r a
runKitchen = handleRelay pure (\k q -> interpret k >>= q)

-- real body of the interpreter, we require SafeIO effect environment (this implies Exc SomeException too)
interpret :: (SafeForRegion Oven r, Member SIO r, Member (Exc SomeException) r)  => Kitchen x -> Eff r x
interpret (MakePizza pizza) = do
        safeIO $ print $ "making pizza " ++ show pizza
        safelyBake $ withOven (OvenId 1)$ \oven -> do
            let ovenName = unsafeWithResource oven show
            safeIO $ putStrLn $ "baking " ++ show pizza ++ " at " ++ ovenName
            _ <- safeIO $ throwIO Overflow
            safeIO $ putStrLn "If this runs, this means that things didn't go on fire"
        return 12
interpret (Complain complaint) = safeIO (print complaint)
