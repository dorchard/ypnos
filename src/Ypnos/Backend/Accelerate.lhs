> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeOperators,
>              TypeFamilies, GADTs, FunctionalDependencies, UndecidableInstances, ScopedTypeVariables #-}

Based on the Ypnos Accelerate backend by Sam Pattuzzi (2012-13)
---------------------------------------------------------------

> module Ypnos.Backend.Accelerate(funGPU,GPUStencil(..),GPUGrid(..)) where

> import Ypnos.Backend.CUDA.Expr.Fun -- Special stencil fun macro

> import Data.Array.Accelerate hiding (flatten)
> import Data.Array.Accelerate.Interpreter as Acc
> import Data.Array.Accelerate.Array.Sugar hiding (size, shape)

 import Data.Array.IArray hiding (Array)
 import Data.Array.Unboxed hiding (Array)

> import Ypnos.Core.Boundary
> import Ypnos.Core.Dimensions
> import Ypnos.Core.Grid
> import Ypnos.Core.Types
> import Ypnos.Core.Combinators

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

Map Ypnos dimensions to Accelerate "shapes"

> type family IShape a

> type instance IShape (Dim x) = Z :. Int
> type instance IShape (Dim x :* Dim y) = Z :. Int :. Int

Map Accelerate "shapes" to Ypnos dimensions

> type family IDimension a

> type instance IDimension (Z :. Int) =  Dim X
> type instance IDimension (Z :. Int :. Int) = Dim X :* Dim Y

> data GPUStencil d x y where
>    GPUStencil :: (Stencil (IShape d) x (sten x)) => (sten x -> Exp y) -> GPUStencil d (GPUGrid d b x) y 
>    GPUStencil3x3 :: (Stencil (IShape ((Dim d0) :* (Dim d1))) x (Stencil3x3 x)) => (Stencil3x3 x -> Exp y) -> GPUStencil ((Dim d0) :* (Dim d1)) (GPUGrid ((Dim d0) :* (Dim d1)) b x) y 

> data GPUGrid d b a where
>     GPUGrid ::
>                Dimensionality d 
>             -> BoundaryList GPUGrid b d a
>             -> Array (IShape d) a
>             -> GPUGrid d b a

> type instance ElemInv (GPUGrid sh) x = Elt x 

Convert between arrays and GPUGrid format


 toArray :: Shape sh => GPUGrid (IDimension sh) b a -> Array sh a
 toArray (GPUGrid _ arr) = arr

boundary :: (Shape sh) =>
            GPUGrid sh b y ->
            BoundaryList b (IDimension sh) y
boundary (GPUGrid b _) = b

> class Conv i sh | i -> sh where
>   fromDim :: i -> sh
> instance Conv (Int) (Z :. Int) where
>   fromDim (x) = (Z :. x)
> instance Conv (Int,Int) (Z :. Int :. Int) where
>   fromDim (x, y) = Z :. x :. y
> instance Conv (Int,Int,Int) (Z :. Int :. Int :. Int) where
>   fromDim (x, y, z) = Z :. x :. y :. z 

> instance GridConstructor GPUGrid where

>  type ConstInv GPUGrid d b a = (Elt a, Shape (IShape d), Conv (Index d) (IShape d))

>  listGrid dim (start, end) ls bound = GPUGrid dim bound arr
>                                         where arr = fromList sh ls
>                                               sh = fromDim end

>  type IxConst GPUGrid d b a = Conv (Index d) (IShape d)

>  (GPUGrid _ _ arr) !!! i = indexArray arr (fromDim i)

> instance Data (GPUGrid sh) where
>     type DataInv (GPUGrid sh) x = ()

>     getData (GPUGrid _ _ arr) = toList arr

> instance Run (GPUGrid d) (GPUStencil d) where
>
>   type RunInv (GPUGrid d) (GPUStencil d) b x y =
>      (Elt y) -- , Stencil (IShape d) x (Stencil3x3 x))

Stencil (IShape d) x (Stencil3x3 x)) -- currently a bit restricted

>   runA (GPUStencil f) (GPUGrid d b a)
>            = GPUGrid d b (Acc.run . sten . use $ a)
>                 where sten = stencil f Clamp

>   run (GPUStencil f) (GPUGrid d b a)
>            = GPUGrid d NilB (Acc.run . sten . use $ a)
>                 where sten = stencil f Clamp


> {-                       
> runG :: forall x y d sh sten . 
>     (IArray UArray x, IArray UArray y,
>     Dimension d,
>     Stencil sh x sten,
>     Elt (Index d), Elt x, Elt y,
>    (EltRepr (Index d)) ~ (EltRepr sh),
>    (IShape d) ~ sh) =>
>    (sten -> Exp y)
>    -> Grid d Nil x
>    -> Grid d Nil y
> runG f (Grid arr d c (b1, b2) boundaries) =
>     Grid (toIArray res) d c (b1, b2) NilB
>     where res = Acc.run (sten_acc) :: Array sh y
>           sten_acc = stencil f (Mirror) acc
>           acc = use arr' :: Acc (Array sh x)
>           arr' = fromIArray arr :: Array sh x
> -}

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
