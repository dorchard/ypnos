{-# LANGUAGE FlexibleContexts, QuasiQuotes #-}
module Average (runAvg) where 

import Prelude hiding (zipWith)
import Ypnos hiding (fun, run)
import Ypnos.CUDA.Expr.Fun 
import Data.Array.Accelerate hiding (flatten)
import Data.Array.Accelerate.Interpreter 

avg :: Floating (Exp a) => Stencil3x3 a -> Exp a
avg ((a, b, c),
     (d, e, f),
     (g, h, i)) = (a + b + c + d + e + f + g + h + i)/9

runAvg :: (IsFloating a, Elt a) => [a] -> Int -> Int -> [a]
runAvg xs x y = toList $ run (stencil avg Clamp acc_xs)
    where acc_xs = use xs'
          xs' = fromList (Z :. x :. y) xs
          
-- Why does this not work?
-- avg' = [fun| Z:|a @b c| -> a + b + c|]:: Floating (Exp a) => Stencil3 a -> 
-- Exp a

avg' :: Floating (Exp a) => Stencil3 a -> Exp a
avg' = [fun| Z:|a @b c| -> (a + b + c)/3|]

-- TODO: implement the Ypnos function for run
runAvg' :: (IsFloating a, Elt a) => [a] -> Int -> [a]
runAvg' xs x = toList $ run (stencil avg' Clamp acc_xs)
    where acc_xs = use xs'
          xs' = fromList (Z :. x) xs
