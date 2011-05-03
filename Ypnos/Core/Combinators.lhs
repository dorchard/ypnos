> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE FunctionalDependencies #-}


> module Ypnos.Core.Combinators where

> import Ypnos.Core.Grid

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import Data.List

> import Debug.Trace

> -- Indexing

The following is desugared from !!! inside a boundary macro

> ypnosReservedBoundaryIndex :: (IArray UArray a, Dimension d) => Grid d (Nil, Static) a -> Index d -> a
> ypnosReservedBoundaryIndex (Grid arr _ _ _ _) i = arr!i

Hidden by grid patterns

> {-# INLINE index1D #-}
> index1D :: (InBoundary (Nat n) b, IArray UArray a) => Nat n -> Grid (Dim d) b a -> a
> index1D n (Grid arr _ c _ _) = arr!(c + (natToInt n))

> {-# INLINE index2D #-}
> index2D :: (InBoundary (Nat n, Nat n') b, IArray UArray a) => (Nat n, Nat n') -> Grid (Dim d :* Dim d') (b, dyn) a -> a
> index2D (n, n') (Grid arr d (x, y) _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + natToInt n, y + natToInt n'))

> {-# INLINE index3D #-}
> index3D :: (InBoundary (Nat n, Nat n', Nat n'') b, IArray UArray a) => (Nat n, Nat n', Nat n'') -> Grid (Dim d :* (Dim d' :* Dim d'')) b a -> a
> index3D (n, n', n'') (Grid arr _ (x, y, z) _ _) = arr!(x + natToInt n, y + natToInt n', z + natToInt n'')

> {-# INLINE indexC #-}
> indexC :: (Dimension d, IArray UArray a) => Grid d b a -> a
> indexC (Grid arr _ c _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) c)

> -- Deconstructor

> gridData :: (IArray UArray a, Dimension d, Ord (Index d)) => Grid d b a -> [a]
> gridData (Grid arr _ _ (origin, extent) _) = 
>             let invert' = \(i, a) -> (invert i, a)
>                 xs = map invert' (sortBy (\(i, _) -> \(i', _) -> compare i i') (map invert' $ assocs arr))  
>                 xs' = filter (\(i, a) -> i >= origin && i <= extent) xs
>             in 
>               map snd xs'

> -- Constructors

> listGrid :: (IArray UArray a, Dimension d) => Dimensionality d -> Index d -> Index d -> [a] -> Grid d (Nil, Static) a
> listGrid d origin extent xs = Grid arr d origin (origin, (dec extent)) NilB
>                                  where arr = listArray (origin, (dec extent)) xs

> grid :: (IArray UArray a, Dimension d) => Dimensionality d -> Index d -> Index d -> [(Index d, a)] -> Grid d (Nil, Static) a
> grid d origin extent xs = Grid arr d origin (origin, (dec extent)) NilB
>                              where arr = array (origin, (dec extent)) xs

> gridWithBoundaries :: (IArray UArray a, Dimension d, 
>                  ReifiableIx upper (Index d),
>                  ReifiableIx lower (Index d)) =>
>                 Dimensionality d -> Index d -> Index d -> [(Index d, a)] ->
>                 BoundaryList ixs dyn lower upper d a ->
>                 Grid d (SafeRelativeIndices ixs, dyn) a

> gridWithBoundaries d origin extent xs boundaries =
>              Grid arr d origin (origin, (dec extent)) boundaries
>              where 
>                 g0 = grid d origin extent xs
>                 es = boundMap d boundaries g0 origin extent 
>                 origin' = add origin (typeToIntIx $ getLowerIx boundaries)
>                 extent' = add extent (typeToIntIx $ getUpperIx boundaries)
>                 arr = array (origin', (dec extent')) (es++xs)

> listGridWithBoundaries :: (IArray UArray a, Dimension d,
>                  ReifiableIx upper (Index d),
>                  ReifiableIx lower (Index d)) =>
>                  Dimensionality d -> Index d -> Index d -> [a] ->
>                  BoundaryList ixs dyn lower upper d a ->
>                  Grid d (SafeRelativeIndices ixs, dyn) a
> listGridWithBoundaries d origin extent xs boundaries =
>              Grid arr d origin (origin, (dec extent)) boundaries
>              where 
>                 g0 = listGrid d origin extent xs
>                 es = boundMap d boundaries g0 origin extent 
>                 origin' = add origin (typeToIntIx $ getLowerIx boundaries)
>                 extent' = add extent (typeToIntIx $ getUpperIx boundaries)
>                 xs' = zip (map invert (range (invert $ origin, invert $ (dec extent)))) xs
>                 arr = array (origin', (dec extent')) (es++xs')


> -- Run stencil computations

> run :: (IArray UArray y, Dimension d) => (Grid d b x -> y) -> Grid d b x -> Grid d (Nil, Static) y
> run f (Grid arr d c (b1, b2) boundaries) =
>            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                arr' = array (b1, b2) dats'
>            in Grid arr' d c (b1, b2) NilB

> class RunGridA dyn where
>     runA :: (IArray UArray a, Dimension d) =>
>                 (Grid d (b, dyn) a -> a) -> Grid d (b, dyn) a -> Grid d (b, dyn) a

> instance RunGridA (Dynamic (Grid d (Nil, Static) a)) where
>     runA f (Grid arr d c (b1, b2) boundaries) = 
>            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                arr' = accum (curry snd) arr dats'
>                g0 = Grid arr' d c (b1, b2) NilB
>                es = boundMap d boundaries g0 b1 b2
>                (b1', b2') = bounds arr
>                arr'' = array (b1', b2') (dats'++es)
>            in
>                Grid arr'' d c (b1, b2) boundaries


> instance RunGridA Static where
>     runA f (Grid arr d c (b1, b2) boundaries) = 
>            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                arr' = accum (curry snd) arr dats'
>            in  Grid arr' d c (b1, b2) boundaries

