{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Exception (evaluate)
import           Eff               (Eff, Member, handleRelay, run)
import           Eff.Exc           (Exc, throwError)
import           Eff.Exc.Pure      (catchError, runError)
import           Eff.SafeIO        (SIO, safeIO)
import           Eff.State.Pure
import           Language.DB       (DB (..))
import           Language.Kitchen  (Kitchen (..), complain)
import           Scenarios
import           Test.Hspec

data TestState = TestState Int Int

runDB :: (Member (State TestState) effs) => Eff (DB ': effs) a -> Eff effs a
runDB = handleRelay pure (\k q -> interpretDB k >>= q)

interpretDB :: (Member (State TestState) effs)  => DB x -> Eff effs x
interpretDB BeginTransaction = do
    modify $ \(TestState complaints transactions) -> TestState complaints (transactions + 1)
    return 1
interpretDB RollbackTransaction = return ()
interpretDB CommitTransaction = return ()

runKitchen :: (Member (State TestState) effs) => Eff (Kitchen ': effs) a -> Eff effs a
runKitchen = handleRelay pure (\k q -> interpretKitchen k >>= q)

interpretKitchen :: (Member (State TestState) effs) => Kitchen x -> Eff effs x
interpretKitchen (Complain _) = do
    modify $ \(TestState complaints transactions) -> TestState (complaints + 1) transactions

runScenario :: Eff '[Kitchen, DB, State TestState] w -> TestState
runScenario = run . (execState (TestState 0 0)) . runDB . runKitchen

main :: IO ()
main = hspec $
    describe "dbTransactions5" $
        it "complains four times" $ do
            let (TestState complaints transactions) = runScenario dbTransactions5
            complaints `shouldBe` 4
            transactions `shouldBe` 1


