module Testing.Ypnos.CUDA.Expr.Fun where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.List

fun_tests = testGroup "Ypnos.CUDA.Expr.Fun" [
                testProperty "sort1" prop_sort1,
                testProperty "sort2" prop_sort2,
                testProperty "sort3" prop_sort3
            ]

prop_sort1 xs = sort xs == sortBy compare xs
  where types = (xs :: [Int])

prop_sort2 xs =
        (not (null xs)) ==>
        (head (sort xs) == minimum xs)
  where types = (xs :: [Int])

prop_sort3 xs = (not (null xs)) ==>
        last (sort xs) == maximum xs
  where types = (xs :: [Int])

