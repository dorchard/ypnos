{-# LANGUAGE FlexibleContexts #-}
module Average (runAvg) where 

import Prelude hiding (zipWith)
import Data.Array.Accelerate hiding (flatten)
import Data.Array.Accelerate.Interpreter 

avg :: Num (Exp a) => Stencil3x3 a -> Exp a
avg ((a, b, c),
     (d, e, f),
     (g, h, i)) = a + b + c + d + e + f + g + h + i

runAvg :: [[Int]] -> Int -> Int -> [[Int]]
runAvg xs x y = toList $ run (stencil avg Clamp acc_xs)
    where acc_xs = use xs'
          xs' = fromList (Z :. x :. y) xs
