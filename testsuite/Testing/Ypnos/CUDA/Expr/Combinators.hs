{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}

module Testing.Ypnos.CUDA.Expr.Combinators where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.List

import Ypnos 
import Ypnos.CUDA
import Ypnos.Core.Grid

import Data.Array.Accelerate hiding (fst, snd, size, fromIntegral)
import qualified Data.Array.Accelerate.Interpreter as I

import Data.Array.Unboxed hiding (Array)

import Control.Monad

comb_tests = testGroup "Ypnos.CUDA.Expr.Combinators"
    [ testProperty "Reduce" prop_reduce
    , testProperty "Run against accelerate" prop_run
    --, testProperty "Run against original Ypnos" prop_run2
    ]

red :: Shape sh => (a -> a -> a) -> a -> Array sh a -> a
red f d a = foldr f d (toList a)

bounded l x y = upper l [x,y] && lower 0 [x,y]
upper l = all (\ x -> x < l)
lower l = all (\ x -> x >= l)

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
runAvg xs = I.run (stencil avg Mirror acc_xs)
    where acc_xs = use xs

avgY :: Floating (Exp a) => Stencil3x3 a -> Exp a
avgY = [funCUDA| X*Y:|a  b c|
                 |d @e f|
                 |g  h i| 
        -> (a + b + c + d + e + f + g + h + i)/9|]

runAvgY :: (Array DIM2 Float) -> (Array DIM2 Float)
runAvgY xs = runG (Sten avgY) xs

gx g = fst (size g) 
gy g = snd (size g)

mirror = [boundary| Float  (*i, -1) g -> g!!!(i, 0) -- top
                          (-1, *j) g -> g!!!(0, j) -- left
                          (+1, *j) g -> g!!!(gx g, j) -- right
                          (*i, +1) g -> g!!!(i, gy g)
                          (-1, -1) g -> g!!!(0, 0) -- top corners
                          (+1, -1) g -> g!!!(gx g, 0) -- top corners
                          (-1, +1) g -> g!!!(0, gy g) -- top corners
                          (+1, +1) g -> g!!!(gx g, gy g) |]

zeroBound = [boundary| Float from (-1, -1) to (+1, +1) -> 0.0 |]


avgY' = [safeFun| X*Y:|a  b c|
                      |d @e f|
                      |g  h i| 
        -> (a + b + c + d + e + f + g + h + i)/9|]

runAvgY' :: [Float] -> (Int,Int) -> [Float]
runAvgY' xs (x, y) = gridData $ run avgY' xs'
    where xs' = listGrid (Dim X :* Dim Y) (0, 0) (x+1, y+1) (cycle xs) mirror
    -- TODO: this should eventually use mirror.

raiseToList :: ((Array DIM2 Float) -> (Array DIM2 Float)) 
            -> [Float] -> (Int,Int) -> [Float]
raiseToList f xs (x,y) = toList $ f arr
    where arr = fromList (Z :. x :. y) xs'
          xs' = cycle xs

runner :: ([Float] -> (Int,Int) -> [Float])
       -> ([Float] -> (Int,Int) -> [Float])
       -> [Float] -> (Int,Int) -> Gen Prop
runner run1 run2 xs (x, y) = upper 10 [x, y] && lower 2 [x, y] && length xs > 0 ==>
    run1 xs (x,y) == run2 xs (x,y)

prop_run = runner (raiseToList runAvg) (raiseToList runAvgY)
prop_run2 = runner (runAvgY') (raiseToList runAvgY)
