import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List


main = defaultMain tests

tests = [
        testGroup "Sorting Group 1" [
                testProperty "sort1" prop_sort1,
                testProperty "sort2" prop_sort2,
                testProperty "sort3" prop_sort3
            ],
        testGroup "Sorting Group 2" [
                testProperty "sort4" prop_sort4,
                testProperty "sort5" prop_sort5,
                testProperty "sort6" prop_sort6,
                testCase "sort7" test_sort7,
                testCase "sort8" test_sort8
            ]
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

prop_sort4 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs ++ ys)) == min (minimum xs) (minimum ys))
  where types = (xs :: [Int], ys :: [Int])

prop_sort5 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
  where types = (xs :: [Int], ys :: [Int])

prop_sort6 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (last (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
  where types = (xs :: [Int], ys :: [Int])

test_sort7 = sort [8, 7, 2, 5, 4, 9, 6, 1, 0, 3] @?= [0..9]

test_sort8 = error "This test deliberately contains a user error"
