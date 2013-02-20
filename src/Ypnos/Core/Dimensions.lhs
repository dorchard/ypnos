> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE EmptyDataDecls #-}

> module Ypnos.Core.Dimensions where

> import Data.Array.IArray

Dimensions

> data X = X
> data Y = Y

 data Z = Z

> class DimIdentifier d

> instance DimIdentifier X
> instance DimIdentifier Y

 instance DimIdentifier Z

> class (Ix (Index d), IndexOps (Index d)) => Dimension d
> instance DimIdentifier d => Dimension (Dim d)
> instance (Dimension (Dim d), Dimension (Dim d')) => (Dimension (Dim d :* Dim d'))

instance (Dimension (Dim d), Dimension (Dim d'), Dimension (Dim d'')) => (Dimension (Dim d :* (Dim d' :* Dim d'')))

> data Dim d 
> data (:*) d d'

> data Dimensionality d where
>     Dim :: d -> Dimensionality (Dim d)
>     (:*) :: Dimensionality (Dim d) -> Dimensionality d' -> Dimensionality (Dim d :* d')

Indices terms

> type family Index t
> type instance Index () = ()
> type instance Index (Dim d) = Int
> type instance Index ((Dim d) :* (Dim d')) = (Int, Int)
> type instance Index ((Dim d) :* ((Dim d') :* (Dim d''))) = (Int, Int, Int)
> type instance Index ((Dim x) :* ((Dim y) :* ((Dim z) :* (Dim w)))) = (Int, Int, Int, Int)
> type instance Index ((Dim x) :* ((Dim y) :* ((Dim z) :* ((Dim w) :* (Dim u))))) = (Int, Int, Int, Int, Int)

Various operations on indices

> -- some Num functionality
> class IndexOps ix where
>     dec :: ix -> ix                  -- decrement an index
>     add :: ix -> ix -> ix            -- add two indices
>     invert :: ix -> ix               -- transpose the index

> instance IndexOps Int where
>     dec x = x - 1
>     add x y = x + y
>     invert = id

> instance IndexOps (Int, Int) where
>     dec (x, y) = (x - 1, y - 1)
>     add (x, a) (y, b) = (x+y, a+b)
>     invert (x, y) = (y, x)

> instance IndexOps (Int, Int, Int) where
>     dec (x, y, z) = (x -1, y - 1, z - 1)
>     add (x, a, u) (y, b, v) = (x+y, a+b, u+v)
>     invert (x, y, z) = (z, y, x)

> instance IndexOps (Int, Int, Int, Int) where
>     dec (x, y, z, w) = (x - 1, y - 1, z - 1, w - 1)
>     add (x, a, u, f) (y, b, v, g) = (x+y, a+b, u+v, f+g)
>     invert (x, y, z, w) = (w, z, y, x)

> instance IndexOps (Int, Int, Int, Int, Int) where
>     dec (x, y, z, w, a) = (x - 1, y - 1, z - 1, w - 1, a - 1)
>     add (x, a, u, f, i) (y, b, v, g, j) = (x+y, a+b, u+v, f+g, i+j)
>     invert (x, y, z, w, a) = (a, w, z, y, x)

> class PointwiseOrd a where
>     lte :: a -> a -> Bool
>     gte :: a -> a -> Bool

> instance PointwiseOrd Int where
>     lte a x = a <= x
>     gte a x = a >= x

> instance PointwiseOrd (Int, Int) where
>     lte (a, b) (x, y) = (a <= x) && (b <= y)
>     gte (a, b) (x, y) = (a >= x) && (b >= y)

> instance PointwiseOrd (Int, Int, Int) where
>     lte (a, b, c) (x, y, z) = (a <= x) && (b <= y) && (c <= z)
>     gte (a, b, c) (x, y, z) = (a >= x) && (b >= y) && (c >= z)


