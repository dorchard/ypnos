{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Testing.Ypnos.CUDA.Expr.Combinators where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.List

import Ypnos.CUDA

import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Interpreter as I

import Control.Monad

comb_tests = testGroup "Ypnos.CUDA.Expr.Combinators"
    [ testProperty "Reduce" prop_reduce
    , testProperty "Run" prop_run]

red :: Shape sh => (a -> a -> a) -> a -> Array sh a -> a
red f d a = foldr f d (toList a)

bounded l x y = l > x && x >= 0 && l > y && y >= 0

prop_reduce :: [Int] -> Int -> Int -> Gen Prop
prop_reduce xs x y =  bounded 50 x y && (length xs) > 0 ==> 
    red (+) 0 arr == (reduceG reducer arr)
    where reducer = mkReducer (Fun2A (+)) (Fun2A (+)) 0 (Fun1A id) 
          arr = fromList (Z :. x :. y) (cycle xs) 

avg :: Floating (Exp a) => Stencil3x3 a -> Exp a
avg ((a, b, c),
     (d, e, f),
     (g, h, i)) = (a + b + c + d + e + f + g + h + i)/9

runAvg :: (IsFloating a, Elt a) => 
          (Array DIM2 a) -> (Array DIM2 a)
runAvg xs = I.run (stencil avg Clamp acc_xs)
    where acc_xs = use xs

avgY :: Floating (Exp a) => Stencil3x3 a -> Exp a
avgY = [fun| X*Y:|a  b c|
                 |d @e f|
                 |g  h i| 
        -> (a + b + c + d + e + f + g + h + i)/9|]

runAvgY :: (Array DIM2 Float) -> (Array DIM2 Float)
runAvgY xs = runG (Sten avgY) xs

raiseToList :: ((Array DIM2 Float) -> (Array DIM2 Float)) 
            -> [Float] -> (Int,Int) -> [Float]
raiseToList f xs (x,y) = toList $ f arr
    where arr = fromList (Z :. x :. y) xs'
          xs' = cycle xs

prop_run :: [Float] -> (Int, Int) -> Gen Prop
prop_run xs (x,y) = bounded 10 x y && length xs > 0 ==>
    runAvg' xs (x,y) == runAvgY' xs (x,y)
    where runAvg' = raiseToList runAvg
          runAvgY' = raiseToList runAvgY
