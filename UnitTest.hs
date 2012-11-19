{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}


module Example1 where

import Ypnos
import Ypnos.Core.Grid
import Test.QuickCheck
import Test.QuickCheck.All

-- Example 1
dat = [1,2,3]::[Int]

datBound = [boundary| Int -1 g -> g!!!0 
                          +1 g -> g!!!2 |]

grid1 xs = listGrid (Dim X) 0 3 xs datBound

f = [fun| X:| a @b c | -> a+b+c |]

prop_grid = forAll (vector 3) $ \xs ->
		   let ys' = run f (grid1 xs) 
		   in 
                       (gridData ys') !! 1 == sum xs

-- Lets
letf = [fun| X:| @a | -> let x = a in x |]

prop_let xs = let ys' = run letf (grid1 xs) in all (\ (x,y) -> x == y) (gridData ys' `zip` xs)

-- Vars
-- Const
constf :: Grid (Dim X) b dyn Int -> Int
constf = [fun| X:| @_ | -> 2 |]

prop_const xs = let ys' = run constf (grid1 xs) in all (\ x -> x == 2) (gridData ys')

-- Tuple
tupf :: Grid (Dim X) b dyn Int -> (Int, Int, Int)
tupf = [fun| X:| a @b c | -> (a, b, c) |]

overlap0 xs = all (\ ((_,a,_),(_,a',_)) -> a == a') (zip xs xs)

prop_tup xs = let ys' = gridData (run tupf (grid1 xs)) in overlap0 ys'

-- Prj
-- Array indices
-- Cond
-- Iterate
-- PrimConst
-- PrimApp
--
-- Operators from Num
-- Operators from Integral and Bits
-- Operators from Floating
-- Relational and inequality
-- Logical operators
-- Character conversion

-- Run all tests
main = $(quickCheckAll)
