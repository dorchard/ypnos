{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ypnos.Examples.Stencils where

import Ypnos
import Ypnos.CUDA
import Ypnos.Core.Grid

import Ypnos.Core.Types --TODO: remove

import Data.Array.Accelerate hiding (fst, snd, size, fromIntegral, map, not)
import qualified Data.Array.Accelerate.Interpreter as I

import Data.Array.Unboxed hiding (Array)

import Control.Monad

import Data.List (unfoldr)

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
runF sten xs (x, y) = gridData $ (runG sten xs')
    where xs' = listGrid (Dim X :* Dim Y) (0, 0) (x+1, y+1) (cycle xs) mirror

avgY = [funGPU| X*Y:|a  b c|
                    |d @e f|
                    |g  h i| -> (a + b + c + d + e + f + g + h + i)/9|]

runAvgY = runF (GPUArr avgY)

-- Ypnos CPU
runF' sten xs (x, y) = gridData $ (runG sten xs')
    where xs' = listGrid (Dim X :* Dim Y) (0, 0) (x+1, y+1) (cycle xs) mirror

avgY' = [funCPU| X*Y:|a  b c|
                    |d @e f|
                    |g  h i| -> (a + b + c + d + e + f + g + h + i)/9|]

gx g = fst (size g)
gy g = snd (size g)

mirror = [boundary| Float (*i, -1) g -> g!!!(i, 0) -- top
                          (-1, *j) g -> g!!!(0, j) -- left
                          (+1, *j) g -> g!!!(gx g, j) -- right
                          (*i, +1) g -> g!!!(i, gy g)
                          (-1, -1) g -> g!!!(0, 0) -- top corners
                          (+1, -1) g -> g!!!(gx g, 0) -- top corners
                          (-1, +1) g -> g!!!(0, gy g) -- top corners
                          (+1, +1) g -> g!!!(gx g, gy g) |]

--zeroBoundF = [boundary| Float from (-1, -1) to (+1, +1) -> 0.0 |]

runAvgY' = runF' (CPUArr avgY')

raiseToList :: ((Array DIM2 Float) -> (Array DIM2 Float))
            -> [Float] -> (Int,Int) -> [Float]
raiseToList f xs (x,y) = toList $ f arr
    where arr = fromList (Z :. x :. y) xs'
          xs' = cycle xs

-- Game of Life

count :: [Exp Bool] -> Exp Int
count = sum . (map (\ x -> x ? (1, 0)))

life = [funGPU| X*Y:| a  b  c |
                    | d @e  f |
                    | g  h  i | ->
                      let n = count ([a, b, c, d, f, g, h, i] :: [Exp Bool]) in
                        (n ==* 3) ||* ((1 <* n) &&* (n <* 4) &&* e) |]

mirrorB = [boundary| Bool (*i, -1) g -> g!!!(i, 0) -- top
                         (-1, *j) g -> g!!!(0, j) -- left
                         (+1, *j) g -> g!!!(gx g, j) -- right
                         (*i, +1) g -> g!!!(i, gy g)
                         (-1, -1) g -> g!!!(0, 0) -- top corners
                         (+1, -1) g -> g!!!(gx g, 0) -- top corners
                         (-1, +1) g -> g!!!(0, gy g) -- top corners
                         (+1, +1) g -> g!!!(gx g, gy g) |]

--zeroBoundB = [boundary| Bool from (-1, -1) to (+1, +1) -> False |]

runLife :: [Bool] -> (Int,Int) -> [Bool]
runLife xs (x, y) = gridData $ runG (GPUArr life) xs'
    where xs' = listGrid (Dim X :* Dim Y) (0, 0) (x, y) (cycle xs) mirrorB

count' = sum . (map (\ x -> if x then 1 else 0))

life' = [funCPU| X*Y:| a  b  c |
                     | d @e  f |
                     | g  h  i | ->
                       let n = count' ([a, b, c, d, f, g, h, i]) in
                         (n == 3) || ((1 < n) && (n < 4) && e) |]

runLife' :: [Bool] -> (Int,Int) -> [Bool]
runLife' xs (x, y) = gridData $ runG (CPUArr life') xs'
    where xs' = listGrid (Dim X :* Dim Y) (0, 0) (x, y) (cycle xs) mirrorB

lifer f (LGrid w h l) = LGrid w h (f l (h, w))

data LifeGrid = LGrid Int Int [Bool]

instance Show LifeGrid where
  show (LGrid width _ elems) = foldl j "" $ map f $ split width elems
      where j a b = a ++ "\n" ++ b
            f = map g
            g False = '.'
            g _     = '#'

split :: Int -> [a] -> [[a]]
split n = takeWhile (not . null) . unfoldr (Just . splitAt n)

grid :: [String] -> LifeGrid
grid l = LGrid w h (concatMap f l)
        where f = map g
              w = length (l!!0)
              h = length l
              g '.' = False
              g _   = True
