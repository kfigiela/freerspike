module Main where

import           Domain
import           Interpreters      ()
import           Language.Bar      (Bar)
import qualified Language.Bar      as Bar
import           Language.CashDesk (CashDesk)
import qualified Language.CashDesk as CashDesk
import           Language.Kitchen  (Kitchen)
import qualified Language.Kitchen  as Kitchen

main :: IO ()
main = do
    servedWine <- restaurantIO
    print servedWine

-- reusable subscenario
payMyBill :: CashDesk m => m ()
payMyBill = do
    billAmt <- CashDesk.makeBill
    CashDesk.payTheBill 12
    CashDesk.payTheBill $ billAmt - 12
    return ()

-- example scenario
restaurant :: (Kitchen m, Bar m, CashDesk m) => m String
restaurant = do
    waitingTime1 <- Kitchen.orderPizza (Pizza "Margherita" Medium)
    waitingTime2 <- Kitchen.orderPizza (Pizza "Capricora" Small)
    wineName <- Bar.serveWine
    Bar.serveAppetizers $ waitingTime1 + waitingTime2
    Kitchen.complain $ "Food does not match " ++ wineName
    payMyBill
    return wineName

restaurantIO :: IO String
restaurantIO = restaurant
