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

class RunGrid grid sh where
    data Sten a b
    runG :: Sten sh a b -> grid a -> grid b

class ReduceGrid grid a b c where   
    type Fun1 a b :: *
    type Fun2 a b c :: *
    reduceG :: Reducer a b c -> grid a -> c

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

instance RunGrid (Array sh) sh where 
    data Sten sh a b where
        Sten :: (Shape sh, Stencil sh a sten',
                 Elt a, Elt b) =>
                (sten' -> Exp b)
                -> Sten sh a b
    runG (Sten f) = Acc.run . (stencil f Clamp) . use 

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
    Reducer ::   (Fun2 a b b) 
              -> (Fun2 b b b) 
              -> b
              -> (Fun1 b c)
              -> Reducer a b c

mkReducer = Reducer

instance (Shape sh,
         Elt a, Elt b, Elt c) => 
         ReduceGrid (Array sh) a b c where
    type Fun2 a b c = Exp a -> Exp b -> Exp c
    type Fun1 a b = Exp a -> Exp b
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
