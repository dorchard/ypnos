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
prop_reduce xs x y = 100 > x && x >= 0 && 100 > y && y >= 0 && (length xs) > 0 ==> 
    red (+) 0 arr == unexp (reduceG reducer arr)
    where reducer = mkReducer (+) (+) (lift (0::Int)) id 
            :: Reducer (Exp Int) (Exp Int) (Exp Int)
          arr = fromList (Z :. x :. y) (cycle xs) 


