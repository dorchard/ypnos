{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Testing.Ypnos.Sten where

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
