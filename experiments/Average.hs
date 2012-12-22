{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Average (runAvg) where 

import Prelude hiding (zipWith)

import Ypnos hiding (fun, run)
import Ypnos.Core.Grid
import Ypnos.CUDA.Expr.Fun 
import Ypnos.CUDA.Expr.Combinators

import Data.Array.Accelerate hiding (flatten, fromIntegral, floor)
import qualified Data.Array.Accelerate.Interpreter as Acc

import Data.Array.IArray
import Data.Array.Unboxed

avg :: Floating (Exp a) => Stencil3x3 a -> Exp a
avg ((a, b, c),
     (d, e, f),
     (g, h, i)) = (a + b + c + d + e + f + g + h + i)/9

runAvg :: (IsFloating a, Elt a) => [a] -> Int -> Int -> [a]
runAvg xs x y = toList $ Acc.run (stencil avg Clamp acc_xs)
    where acc_xs = use xs'
          xs' = fromList (Z :. x :. y) xs
          
-- Why does this not work?
-- avg' = ([fun| Z:|a @b c| -> a + b + c|]):: Floating (Exp a) => Stencil3 a -> 
--Exp a

avg' :: Floating (Exp a) => Stencil3 a -> Exp a
avg' = [fun| Z:|a @b c| -> (a + b + c)/3|]

--TODO: implement the Ypnos function for run
runAvg' :: forall a. (IsFloating a, Elt a, IArray UArray a) => [a] -> [a]
runAvg' xs = grid2List (run avg' grid)
    where grid = listGrid (Dim X) 0 (length xs) xs NilB :: Grid (Dim X) Nil Static a

grid2List :: (IArray UArray a, Ix (Index d)) => Grid d b dyn a -> [a]
grid2List (Grid acc _ _ _ _) = elems acc

avg2D :: Floating (Exp a) => Stencil3x3 a -> Exp a
avg2D = [fun| X*Y:|a  b c|
                  |d  e f|
                  |g @h i| -> (a + b + c + d + e + f + g + h + i)/9|]

runAvg2D :: forall a. (IsFloating a, Elt a, IArray UArray a) => [a] -> [a]
runAvg2D xs = grid2List (run avg2D grid)
    where grid = listGrid (Dim X :* Dim Y) (0,0) (w,h) xs NilB :: Grid (Dim X :* Dim Y) Nil Static a
          w = intSqrt len :: Int
          h = intSqrt len :: Int
          len = length xs :: Int

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral
