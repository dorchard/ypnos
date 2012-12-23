{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ypnos.CUDA.Expr.Combinators where

import Data.Array.Accelerate hiding (flatten) 
import Data.Array.Accelerate.Interpreter as Acc
import Data.Array.Accelerate.Array.Sugar
import Data.Array.IArray hiding (Array)
import Data.Array.Unboxed hiding (Array)

import Ypnos.Core.Grid

type family IShape a

type instance IShape (Dim x) = Z :. Int
type instance IShape (Dim x :* Dim y) = Z :. Int :. Int

run :: forall x y d sh sten b dyn. 
    (IArray UArray x, IArray UArray y, 
    Dimension d, 
    Stencil sh x sten, 
    Elt (Index d), Elt x, Elt y, 
    (EltRepr (Index d)) ~ (EltRepr sh), 
    (IShape d) ~ sh) => 
    (sten -> Exp y) 
    -> Grid d b dyn x 
    -> Grid d Nil Static y
run f (Grid arr d c (b1, b2) boundaries) =
    Grid (toIArray res) d c (b1, b2) NilB
    where res = Acc.run (sten_acc) :: Array sh y
          sten_acc = stencil f Clamp acc :: Acc (Array sh y)
          acc = use arr' :: Acc (Array sh x)
          arr' = fromIArray arr :: Array sh x
          
--fromIArray' :: UArray (Index d) a -> Array e
