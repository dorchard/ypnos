{-# LANGUAGE FlexibleContexts #-}
module Average (runAvg) where 

import Prelude hiding (zipWith)
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
