{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}


module Ypnos.CUDA.Expr.Combinators (Fun1(..), Fun2(..), GPUArr(..), toArray, fromArray) where

import Data.Array.Accelerate hiding (flatten)
import Data.Array.Accelerate.Interpreter as Acc
import Data.Array.Accelerate.Array.Sugar hiding (size, shape)
import Data.Array.IArray hiding (Array)
import Data.Array.Unboxed hiding (Array)

import Ypnos.Core.Dimensions
import Ypnos.Core.Grid
import Ypnos.Core.Types
import Ypnos.Core.Combinators

import Prelude hiding (map, zipWith, replicate)

-- Grid class

{-class YGrid grid where-}
    {-listGrid :: (Dimension d) => [a] -> (Index d) -> grid-}
    {-gridData :: grid -> [a]-}

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

type family IDimension a

type instance IDimension (Z :. Int) =  Dim X
type instance IDimension (Z :. Int :. Int) = Dim X :* Dim Y

{-instance YGrid (Array sh x) where-}
    {-listGrid = undefined-}
    {-gridData = undefined-}

data GPUArr sh x y where
    GPUArr :: (Stencil sh x sten) =>
           (sten -> Exp y) ->
           GPUArr sh x y

data GPUGrid b dyn lower upper sh x where
  GPUGrid :: BoundaryList b (IDimension sh) x -> Array sh x ->
             GPUGrid b dyn lower upper sh x

fromArray :: (Shape sh) =>
             BoundaryList b (IDimension sh) y -> Array sh y ->
             GPUGrid b dyn lower upper sh y
fromArray = GPUGrid

toArray :: Shape sh => GPUGrid b dyn lower upper sh y -> Array sh y
toArray (GPUGrid _ arr) = arr

boundary :: (Shape sh) =>
            GPUGrid b dyn lower upper sh y ->
            BoundaryList b (IDimension sh) y
boundary (GPUGrid b _) = b

class Conv i sh | i -> sh where
  fromDim :: i -> sh
instance Conv (Int) (Z :. Int) where
  fromDim (x) = (Z :. x)
instance Conv (Int,Int) (Z :. Int :. Int) where
  fromDim (x, y) = Z :. x :. y
--fromDim (Dim _ :* Dim _ :* Dim _) (x, y, z) = Z :. x :. y :. z

instance (d ~ IDimension sh, IShape d ~ sh, Shape sh) =>
         GridList (GPUGrid b dyn lower upper sh) d b where
  type ListConst (GPUGrid b dyn lower upper sh) d b a =
    (Elt a, Conv (Index d) sh)
  type DataConst (GPUGrid b dyn lower upper sh) d b a = ()
  listGrid dim start end ls bound = GPUGrid bound arr
    where arr = fromList sh ls
          sh = fromDim end
  gridData (GPUGrid _ arr) = toList arr
  type IxConst (GPUGrid b dyn lower upper sh) d b a = (Conv (Index d) sh)
  (GPUGrid _ arr) !!! i = indexArray arr i'
    where i' = fromDim i

instance Shape sh => RunGrid (GPUGrid b dyn lower upper sh)
                               (GPUArr sh) where
    type RunCon (GPUGrid b dyn lower upper sh) (GPUArr sh) x y =
      (Elt y, Stencil sh x (Stencil3x3 x), x ~ y)
    runG (GPUArr f) g = fromArray b $ Acc.run $ sten $ use $ toArray g
                     where sten = stencil f Clamp
                           b = boundary g

-- Old, before using type classes
run :: forall x y d sh sten.
    (IArray UArray x, IArray UArray y,
    Dimension d,
    Stencil sh x sten,
    Elt (Index d), Elt x, Elt y,
    (EltRepr (Index d)) ~ (EltRepr sh),
    (IShape d) ~ sh) =>
    (sten -> Exp y)
    -> Grid d Nil x
    -> Grid d Nil y
run f (Grid arr d c (b1, b2) boundaries) =
    Grid (toIArray res) d c (b1, b2) NilB
    where res = Acc.run (sten_acc) :: Array sh y
          sten_acc = stencil f (Mirror) acc
          acc = use arr' :: Acc (Array sh x)
          arr' = fromIArray arr :: Array sh x

--The reduce primitive

instance (Shape sh) => ReduceGrid (Array sh) where

    type ConstFun2 (Array sh) a b c = (Elt a, Elt b, Elt c)
    type Fun2 (Array sh) a b c = (Exp a -> Exp b -> Exp c)
    type ConstFun1 (Array sh) a b = (Elt a, Elt b)
    type Fun1 (Array sh) a b = (Exp a -> Exp b)

    reduceG (Reducer inter comb def conv) grid =
        only $ Acc.run converted
        where
              converted = map conv folded
              folded = foldAll comb def' zipped
              zipped = zipWith inter g defG
              defG = use $ fromList sh (cycle [def])
              sh = arrayShape grid
              g = use grid
              def' = the $ use $ fromList Z [def]

only :: Scalar a -> a
only arr = (toList arr) !! 0
