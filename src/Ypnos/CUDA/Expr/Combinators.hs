{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ypnos.CUDA.Expr.Combinators where

import Data.Array.Accelerate hiding (flatten) 
import Data.Array.Accelerate.Interpreter as Acc
import Data.Array.Accelerate.Array.Sugar hiding (size, shape)
import Data.Array.IArray hiding (Array)
import Data.Array.Unboxed hiding (Array)

import Ypnos.Core.Grid

import Prelude hiding (map, zipWith, fold, replicate)

-- Grid class

{-class YGrid grid where-}
    {-listGrid :: (Dimension d) => [a] -> (Index d) -> grid-}
    {-gridData :: grid -> [a]-}

class RunGrid grid sten where
    runG :: sten -> grid -> grid

class ReduceGrid grid a b c where
    reduceG :: Reducer a b c-> grid -> c

--Utils
{-useGrid :: -}
    {-(IArray UArray x, IArray UArray y, -}
    {-Elt x, -}
    {-(IShape d) ~ sh, -}
    {-(EltRepr (Index d)) ~ (EltRepr sh), -}
    {-Shape sh) => -}
    {-Grid d b dyn x -> Acc (Array sh x)-}
{-useGrid (Grid arr d c (b1, b2) boundaries) = use $ fromIArray arr-}
{-returnGrid :: ((IShape d) ~ sh) => -}
              {-Grid d b dyn y -> Acc (Array sh x) -> -}
              {-Grid d b dyn x-}
{-returnGrid (Grid arr d c (b1, b2) boundaries) acc =-}
    {-Grid (toIArray res) d c (b1, b2) NilB-}
    {-where res = Acc.run acc-}

--The run primitive
type family IShape a

type instance IShape (Dim x) = Z :. Int
type instance IShape (Dim x :* Dim y) = Z :. Int :. Int

{-instance YGrid (Array sh x) where-}
    {-listGrid = undefined-}
    {-gridData = undefined-}

data AStencil sh x where
    AStencil :: Stencil sh x sten => (sten -> Exp x) -> AStencil sh x

instance RunGrid (Array sh x) (AStencil sh x) where 
    runG (AStencil f) = Acc.run . (stencil f Clamp) . use 

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
          sten_acc = stencil f Clamp acc 
          acc = use arr' :: Acc (Array sh x)
          arr' = fromIArray arr :: Array sh x

--The reduce primitive

data Reducer a b c where
    Reducer ::   (a -> b -> b) 
              -> (b -> b -> b) 
              -> b
              -> (b -> c)
              -> Reducer a b c

mkReducer = Reducer

instance (Shape sh,
         Elt a, Elt b, Elt c) => 
         ReduceGrid (Array sh a) (Exp a) (Exp b) (Exp c) where
    reduceG (Reducer inter comb def conv) grid =
        conv folded
        where 
              folded = the $ foldAll comb def zipped
              zipped = zipWith inter g defG
              defG = reshape sh $ replicate (lift (Z :.s )) (unit def)
              s = arraySize sh'
              sh' = arrayShape grid
              sh = shape g
              g = use grid

unexp :: Elt a => Exp a -> a
unexp = only . Acc.run . unit

only :: Scalar a -> a
only arr = (toList arr) !! 0
