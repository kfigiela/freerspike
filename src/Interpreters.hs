module Interpreters where
import           Domain
import           Language.Bar
import           Language.CashDesk
import           Language.Kitchen

instance Kitchen IO where
  orderPizza pizza = do
    print pizza
    return 42
  complain = putStrLn


instance Bar IO where
    serveWine = do
        putStrLn "Serving some wine"
        return "Merlot"
    serveAppetizers time = putStrLn $ "Appetizers for waiting time: " ++ (show time)

instance CashDesk IO where
    makeBill = do
        putStrLn "Printing bill"
        return 421
    payTheBill amt = do
        putStrLn $ "Paid " ++ (show amt)
        return Nothing

