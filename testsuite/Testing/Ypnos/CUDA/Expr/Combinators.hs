{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Testing.Ypnos.CUDA.Expr.Combinators where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.List

import Ypnos.CUDA.Expr.Combinators
import Ypnos.Expr.Expr hiding (Z)

import Data.Array.Accelerate

import Control.Monad

comb_tests = testGroup "Ypnos.CUDA.Expr.Combinators"
    [ testProperty "Reduce" prop_reduce ]

red :: Shape sh => (a -> a -> a) -> a -> Array sh a -> a
red f d a = foldr f d (toList a)

prop_reduce :: [Int] -> Int -> Int -> Gen Prop
prop_reduce xs x y = 50 > x && x >= 0 && 50 > y && y >= 0 && (length xs) > 0 ==> 
    red (+) 0 arr == (reduceG reducer arr)
    where reducer = mkReducer (Fun2 (+)) (Fun2 (+)) 0 (Fun1 id) 
          arr = fromList (Z :. x :. y) (cycle xs) 


