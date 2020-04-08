#!/usr/bin/env runhaskell

{-
 - Russian IP patent optimiser
 -
 - Due to deductions, it is often beneficial to buy a patent not for
 - the entire year. This calculator tells you for how long you should
 - get a patent to optimise your taxes.
 -
 - Fill in the global variables and run the program.
-}


module Main where

import Control.Monad (forM_)
import Data.Decimal (Decimal, roundTo)  -- package: Decimal
import Data.Foldable (maximumBy)
import Data.Ord (comparing)


-----------------------
-- Fill these in
-----------------------

-- | Your expected monthly income in ₽
realIncome = 120000

-- | Price of the patent for 1 month in your region in ₽
--
-- Look it up at <https://patent.nalog.ru>.
patentPrice = 5000  -- this is for St. Petersburg


-----------------------
-- Magic constants (year 2018)
-----------------------

----
-- ФНС
----

-- | Tax rate for УСН and ПСН
taxRate = 6 / 100


----
-- ОПС
----

-- | Fixed
fixedPfr = 32448

-- | Starting from this you pay extra
extraFromPfr = 300000

-- | How much extra you pay
extraRatePfr = 1 / 100

-- | You won’t pay more than this
maxPfr = 259584


----
-- ОМС
----

-- | Fxed
fixedOms = 8426

--------------------------------------------------------------------------------


-- | This is what FNS thinks you get per month when on patent
patentIncome = patentPrice / 6 * 100


-- | What FNS thinks you will get per year
taxedIncomeY :: Int -> Decimal
taxedIncomeY patMonths = realIncome * fromIntegral (12 - patMonths) + patentIncome * fromIntegral patMonths

-- | What you have to pay to ОПС and ОМС
insuranceY :: Int -> Decimal
insuranceY patMonths = fixedOms + fixedPfr + extraPfr
  where
    extraPfr = min maxPfr (over * extraRatePfr)
    over = max 0 (taxedIncomeY patMonths - extraFromPfr)

-- | What you have to pay to ФНС
taxesY :: Int -> Decimal
taxesY patMonths = taxedIncomeY patMonths * taxRate - insuranceDeduction
  where
    insuranceDeduction = min (insuranceY patMonths) (realIncome * fromIntegral (12 - patMonths) * taxRate)

-- | What you make in the end
netIncome :: Int -> Decimal
netIncome patMonths = realIncome - (insuranceY patMonths + taxesY patMonths) / fromIntegral 12


main :: IO ()
main = do
    putStrLn $ "Monthly income: " ++ showMoney realIncome
    putStrLn ""
    putStrLn "Taxes total:"
    forM_ taxesTotal $ \(n, taxes) -> putStrLn $ "  " ++ show n ++ ":\t" ++ showMoney taxes
    putStrLn ""
    putStrLn $ "Best: " ++ show bestN ++ " months with patent; net monthly income = " ++ showMoney bestInc
    putStrLn $ "  Diff per month (no patent):   " ++ showMoney (bestInc - netIncome 0)
    putStrLn $ "  Diff per month (full patent): " ++ showMoney (bestInc - netIncome 12)
  where
    taxesTotal = map (\n -> (n, taxesY n + insuranceY n)) [0..12]
    results = map (\n -> (n, netIncome n)) [0..12]
    (bestN, bestInc) = maximumBy (comparing snd) results

    showMoney = (++ " ₽") . show . roundTo 2
