{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Ypnos.Examples.Stencils where

import Ypnos
import Ypnos.CUDA
import Ypnos.Core.Grid

import Data.Array.Accelerate hiding (fst, snd, size, fromIntegral)
import qualified Data.Array.Accelerate.Interpreter as I

import Data.Array.Unboxed hiding (Array)

import Control.Monad

-- Accelerate directly
avg :: Floating (Exp a) => Stencil3x3 a -> Exp a
avg ((a, b, c),
     (d, e, f),
     (g, h, i)) = (a + b + c + d + e + f + g + h + i)/9

runAvg :: (IsFloating a, Elt a) =>
          (Array DIM2 a) -> (Array DIM2 a)
runAvg xs = I.run (stencil avg Mirror acc_xs)
    where acc_xs = use xs

-- Ypnos GPU (Accelerate)
avgY :: Floating (Exp a) => Stencil3x3 a -> Exp a
avgY = [funCUDA| X*Y:|a  b c|
                      |d @e f|
                      |g  h i| -> (a + b + c + d + e + f + g + h + i)/9|]

runAvgY :: (Array DIM2 Float) -> (Array DIM2 Float)
runAvgY xs = runG (Sten avgY) xs

-- Ypnos CPU
avgY' :: (IArray UArray a, Fractional a,
        InBoundary (IntT (Pos (S Zn)), IntT (Pos (S Zn))) b,
        InBoundary (IntT (Pos (S Zn)), IntT (Pos Zn)) b,
        InBoundary (IntT (Pos (S Zn)), IntT (Neg (S Zn))) b,
        InBoundary (IntT (Pos Zn), IntT (Pos (S Zn))) b,
        InBoundary (IntT (Neg (S Zn)), IntT (Pos (S Zn))) b,
        InBoundary (IntT (Neg (S Zn)), IntT (Pos Zn)) b,
        InBoundary (IntT (Neg (S Zn)), IntT (Neg (S Zn))) b,
        InBoundary (IntT (Pos Zn), IntT (Neg (S Zn))) b) =>
        Grid (Dim d0 :* Dim d'0) b dyn a -> a
avgY' = [funCPU| X*Y:|a  b c|
                    |d @e f|
                    |g  h i| -> (a + b + c + d + e + f + g + h + i)/9|]

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

--zeroBoundF = [boundary| Float from (-1, -1) to (+1, +1) -> 0.0 |]

runAvgY' :: [Float] -> (Int,Int) -> [Float]
runAvgY' xs (x, y) = gridData $ run avgY' xs'
    where xs' = listGrid (Dim X :* Dim Y) (0, 0) (x+1, y+1) (cycle xs) mirror

raiseToList :: ((Array DIM2 Float) -> (Array DIM2 Float))
            -> [Float] -> (Int,Int) -> [Float]
raiseToList f xs (x,y) = toList $ f arr
    where arr = fromList (Z :. x :. y) xs'
          xs' = cycle xs

-- Game of Life

count = length . filter id

life = [funCPU| X*Y:| a  b  c |
                    | d @e  f |
                    | g  h  i | ->
                      let n = count [a, b, c, d, f, g, h, i] in
                        (n == 3) || (1 < n && n < 4 && e) |]

zeroBoundB = [boundary| Bool from (-1, -1) to (+1, +1) -> False |]

runLife :: [Bool] -> (Int,Int) -> [Bool]
runLife xs (x, y) = gridData $ run life xs'
    where xs' = listGrid (Dim X :* Dim Y) (0, 0) (x+1, y+1) (cycle xs) zeroBoundB
