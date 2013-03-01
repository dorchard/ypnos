{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}

module Ypnos.CUDA.Expr.Combinators (Fun1(..), Fun2(..), Arr(..), toArray, fromArray) where

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

data Arr g x y where
    Arr :: Const g (Exp x) => (g (Exp x) -> Exp y) -> Arr g x y

data GPUGrid b sh x

fromArray :: (Shape sh) =>
             BoundaryList b dyn lower upper (IDimension sh) a -> Array sh y ->
             GPUGrid b sh y
fromArray = undefined

toArray :: Shape sh => GPUGrid b sh y -> Array sh y
toArray = undefined

boundary :: (Shape sh) =>
            GPUGrid b sh y -> BoundaryList b dyn lower upper (IDimension sh) a
boundary = undefined

instance (Shape sh) => GridC (GPUGrid b sh) where
  type Const (GPUGrid b sh) a = ()
instance (Shape sh) => Grid1D (GPUGrid b sh) b
instance (Shape sh) => Grid2D (GPUGrid b sh) b

instance (Shape sh) => RunGrid (GPUGrid b sh) Arr where
    type RunCon (GPUGrid b sh) Arr x y = (Elt y, Stencil sh x (Stencil3x3 x))
    runG (Arr f) g = fromArray (boundary g) $ Acc.run $ ((stencil . conv) (f) (Mirror)) $ use $ toArray g

conv :: (Shape sh) => (GPUGrid b sh (Exp x) -> Exp y) -> (Stencil3x3 x -> Exp y)
conv = undefined

-- Old, before using type classes
run :: forall x y d sh sten.
    (IArray UArray x, IArray UArray y,
    Dimension d,
    Stencil sh x sten,
    Elt (Index d), Elt x, Elt y,
    (EltRepr (Index d)) ~ (EltRepr sh),
    (IShape d) ~ sh) =>
    (sten -> Exp y)
    -> Grid d Nil Static x
    -> Grid d Nil Static y
run f (Grid arr d c (b1, b2) boundaries) =
    Grid (toIArray res) d c (b1, b2) NilB
    where res = Acc.run (sten_acc) :: Array sh y
          sten_acc = stencil f (Mirror) acc
          acc = use arr' :: Acc (Array sh x)
          arr' = fromIArray arr :: Array sh x

--The reduce primitive

instance (Shape sh) => ReduceGrid (Array sh) where
    data Fun2 a b c where
        Fun2A :: (Elt a, Elt b, Elt c) =>
                (Exp a -> Exp b -> Exp c)
                -> Fun2 a b c
    data Fun1 a b where
        Fun1A :: (Elt a, Elt b) =>
                (Exp a -> Exp b)
                -> Fun1 a b
    reduceG (Reducer (Fun2A inter)
                     (Fun2A comb)
                     def
                     (Fun1A conv)) grid =
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
