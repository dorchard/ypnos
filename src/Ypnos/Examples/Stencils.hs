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


avg :: Floating (Exp a) => Stencil3x3 a -> Exp a
avg ((a, b, c),
     (d, e, f),
     (g, h, i)) = (a + b + c + d + e + f + g + h + i)/9

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

avgY :: Floating (Exp a) => Stencil3x3 a -> Exp a
avgY = [funCUDA| X*Y:|a  b c|
                      |d @e f|
                      |g  h i| -> (a + b + c + d + e + f + g + h + i)/9|]
count = length . filter id

-- Game of Life

life = [funCPU| X*Y:| a  b  c |
                    | d @e  f |
                    | g  h  i | ->
                      let n = count [a, b, c, d, f, g, h, i] in
                        (n == 3) || (1 < n && n < 4 && e) |]

zeroBound = [boundary| Bool from (-1, -1) to (+1, +1) -> False |]

runLife :: [Bool] -> (Int,Int) -> [Bool]
runLife xs (x, y) = gridData $ run life xs'
    where xs' = listGrid (Dim X :* Dim Y) (0, 0) (x+1, y+1) (cycle xs) zeroBound
