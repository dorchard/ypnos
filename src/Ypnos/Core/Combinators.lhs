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
> {-# LANGUAGE ConstraintKinds #-}

> module Ypnos.Core.Combinators where

> import Ypnos.Core.Grid
> import Ypnos.Core.Dimensions
> import Ypnos.Core.Boundary
> import Ypnos.Core.Types
> import Ypnos.Core.Dimensions

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import GHC.Prim

> import Data.Monoid
> import Data.List

> import Debug.Trace

Ypnos classes.

> class RunGrid g arr | arr -> g where
>     type RunCon g arr x y :: Constraint
>     runG :: RunCon g arr x y =>
>             (x `arr` y)
>             -> g x -> g y

> class GridList g d b | g -> d, g -> b where
>   type ListConst g d b a :: Constraint
>   listGrid :: ListConst g d b a =>
>               Dimensionality d -> Index d -> Index d -> [a] ->
>               BoundaryList b d a ->
>               g a
>   type DataConst g d b  a :: Constraint
>   gridData :: DataConst g d b  a =>
>               g a -> [a]
>   type IxConst g d b a :: Constraint
>   (!!!) :: IxConst g d b a => g a -> Index d -> a


> instance (Dimension d) => GridList (Grid d b) d b where
>   type ListConst (Grid d b) d b a =
>     (IArray UArray a, Dimension d,
>      BoundaryInfo b d)
>   listGrid = listGrid'
>   type DataConst (Grid d b) d b a =
>     (Dimension d, PointwiseOrd (Index d),
>      IArray UArray a)
>   gridData = gridData'
>   type IxConst (Grid d b) d b a =
>     (IArray UArray a, Ix (Index d))
>   (!!!) = ix


 instance (DimIdentifier x, DimIdentifier y)
           => GridC2D (Grid (Dim x :* Dim y) b dyn) where
    type Const (Grid (Dim x :* Dim y) b dyn) a = (IArray UArray a)
    type OrC (Grid (Dim x :* Dim y) b dyn) n n' n'' = Safe (IntT n, IntT n') b
    type I (Grid (Dim x :* Dim y) b dyn) = (Int, Int)
    type Or (Grid (Dim x :* Dim y) b dyn) n n' n'' = (IntT n, IntT n')
    indexCur = indexC
    indexXD = index2D
    unsafeIndexXD = unsafeIndex2D


> class ReduceGrid grid where
>     data Fun1 a b
>     data Fun2 a b c
>     reduceG :: Reducer a c -> grid a -> c

> data Reducer a c where
>     Reducer ::   (Fun2 a b b)
>               -> (Fun2 b b b)
>               -> b
>               -> (Fun1 b c)
>               -> Reducer a c
>
> mkReducer = Reducer

> data CPUArr d b x y where
>   CPUArr :: (Dimension d) =>
>             (Grid d b x -> y) -> CPUArr d b x y

> instance (Dimension d, RunGridA (Dynamism b)) =>
>          RunGrid (Grid d b) (CPUArr d b) where
>     type RunCon (Grid d b) (CPUArr d b) x y =
>       (IArray UArray y, x ~ y)
>     runG (CPUArr f) = runA f

Indexing

The following is desugared from !!! inside a boundary macro

> ypnosReservedBoundaryIndex :: (IArray UArray a, Dimension d) => Grid d Nil a -> Index d -> a
> ypnosReservedBoundaryIndex (Grid arr _ _ _ _) i = arr!i

Hidden by grid patterns

> {-# INLINE index1D #-}
> index1D :: (Safe (IntT n) (Absify b), IArray UArray a) => IntT n -> Int -> Grid (Dim d) b a -> a
> index1D _ n (Grid arr d x _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n))

OLD form

 index1D :: (Safe (IntT n) b, IArray UArray a) => IntT n -> Grid (Dim d) b dyn a -> a
 index1D n (Grid arr _ c _ _) = arr!(c + (intTtoInt n))

> {-# INLINE index2D #-}
> index2D :: (Safe (IntT n, IntT n') (Absify b), IArray UArray a) => (IntT n, IntT n') -> (Int, Int) -> Grid (Dim d :* Dim d') b a -> a
> index2D _ (n, n') (Grid arr d (x, y) _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n, y + n'))

OLD form

 index2D :: (Safe (IntT n, IntT n') b, IArray UArray a) => (IntT n, IntT n') -> Grid (Dim d :* Dim d') b dyn a -> a
 index2D (n, n') (Grid arr d (x, y) _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + intTtoInt n, y + intTtoInt n'))

> {-# INLINE index3D #-}
> index3D :: (Safe (IntT n, IntT n', IntT n'') (Absify b), IArray UArray a) => (IntT n, IntT n', IntT n'') -> Grid (Dim d :* (Dim d' :* Dim d'')) b a -> a
> index3D (n, n', n'') (Grid arr _ (x, y, z) _ _) = arr!(x + intTtoInt n, y + intTtoInt n', z + intTtoInt n'')

> {-# INLINE unsafeIndex2D #-}
> unsafeIndex2D :: (IArray UArray a) => (Int, Int) -> Grid (Dim d :* Dim d') b a -> a
> unsafeIndex2D (n, n') (Grid arr d (x, y) _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n, y + n'))

> {-# INLINE unsafeIndex1D #-}
> unsafeIndex1D :: (IArray UArray a) => Int -> Grid (Dim d) b a -> a
> unsafeIndex1D n (Grid arr d x _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n))

 class RelativeIndex i d where
     index :: (Safe i b, IArray UArray a) => i -> Grid d b dy a -> a
 instance RelativeIndex (IntT n) (Dim d) where
     index = index1D
 instance RelativeIndex (IntT n, IntT n') (Dim d :* Dim d') where
     index = index2D
 instance RelativeIndex (IntT n, IntT n', IntT n'') (Dim d :* (Dim d' :* Dim d'')) where
     index = index3D

> indexC :: (Dimension d, IArray UArray a) => Grid d b  a -> a
> indexC (Grid arr _ c _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) c)

> ix :: (IArray UArray a, Ix (Index d)) => Grid d b a -> Index d -> a
> ix (Grid arr _ _ _ _) i = arr!i

Grid deconstructors

> gridData' :: (Dimension d, PointwiseOrd (Index d), IArray UArray a) => Grid d b a -> [a]
> gridData' (Grid arr _ _ (origin, extent) _) =
>             let invert' = \(i, a) -> (invert i, a)
>                 xs = map invert' (sortBy (\(i, _) -> \(i', _) -> compare i i') (map invert' $ assocs arr))
>                 xs' = filter (\(i, a) -> gte i origin && lte i extent) xs
>             in
>               map snd xs'

> class (Dimension d) => Size d where
>     size :: Grid d b a -> (Index d)

> instance (DimIdentifier d) => Size (Dim d) where
>     size (Grid _ _ _ (l, b) _) = b - l

> instance (DimIdentifier d, DimIdentifier d') => Size ((Dim d) :* (Dim d')) where
>     size (Grid _ _ _ ((lx,ly), (ux,uy)) _) = (ux-lx, uy-ly)


Grid constructors

> listGridNoBoundary :: (IArray UArray a, Dimension d) => Dimensionality d -> Index d -> Index d -> [a] -> Grid d Nil a
> listGridNoBoundary d origin extent xs = Grid arr d origin (origin, (dec extent)) NilB
>                                  where arr = listArray (origin, (dec extent)) xs

> gridNoBoundary :: (IArray UArray a, Dimension d) => Dimensionality d -> Index d -> Index d -> [(Index d, a)] -> Grid d Nil a
> gridNoBoundary d origin extent xs = Grid arr d origin (origin, (dec extent)) NilB
>                              where arr = array (origin, (dec extent)) xs


> grid :: (BoundaryInfo ixs d, IArray UArray a, Dimension d) =>
>         Dimensionality d -> Index d -> Index d -> [(Index d, a)] ->
>         BoundaryList ixs d a ->
>         Grid d ixs a

> grid d origin extent xs boundaries =
>              Grid arr d origin (origin, (dec extent)) boundaries
>              where
>                 g0 = gridNoBoundary d origin extent xs
>                 es = boundMap d boundaries g0 origin extent
>                 origin' = add origin (lowerIx boundaries)
>                 extent' = add extent (upperIx boundaries)
>                 arr = array (origin', (dec extent')) (es++xs)

> listGrid' :: (BoundaryInfo ixs d, IArray UArray a, Dimension d) =>
>                  Dimensionality d -> Index d -> Index d -> [a] ->
>                  BoundaryList ixs d a ->
>                  Grid d ixs a
> listGrid' d origin extent xs boundaries =
>   Grid arr d origin (origin, (dec extent)) boundaries
>              where
>                 g0 = listGridNoBoundary d origin extent xs
>                 es = boundMap d boundaries g0 origin extent
>                 origin' = add origin (lowerIx boundaries)
>                 extent' = add extent (upperIx boundaries)
>                 xs' = zip (map invert (range (invert $ origin, invert $ (dec extent)))) xs
>                 arr = array (origin', dec extent') (es++xs')

Ziping and unzipping grids

> gridZip :: (IArray UArray x, IArray UArray y, IArray UArray (x, y),
>             BZip b b' b'' d,
>             Dimension d) =>
>            Grid d b x -> Grid d b' y -> Grid d b'' (x, y)
> gridZip (Grid arr d c (l, u) b) (Grid arr' _ c' (l', u') b')
>     | (l /= l') || (u /= u') = error "Can only zip grids of the same size"
>     | c /= c'                = error "Can only zip grids with the same cursor"
>     | otherwise              = let arr'' = array (l, u) (map (\((i,x),(i',y)) -> (i, (x, y))) (zip (assocs arr) (assocs arr')))
>                                    b'' = bzip b b'
>                                in (Grid arr'' d c (l, u) b'')

> gridUnzip :: (BUnzip b, Functor (Grid d Nil), Ix (Index d),
>               IArray UArray x, IArray UArray y, IArray UArray (x, y)) =>
>              Grid d b (x, y) -> (Grid d b x, Grid d b y)
> gridUnzip (Grid arr d c (l, u) b) =
>     let (b', b'') = bUnzip b
>     in (Grid (amap fst arr) d c (l, u) b', Grid (amap snd arr) d c (l, u) b'')

Run stencil computations

> data Reduce r x = Reduce r x

> runReduceSimple :: (IArray UArray y, Dimension d, Monoid r) =>
>                    (Grid d b x -> Reduce r y)
>                 -> Grid d b x
>                 -> Reduce r (Grid d Nil y)
> runReduceSimple f (Grid arr d c (b1, b2) bndrs) =
>          let g = (\(Reduce r as) -> \c' -> let Reduce r' x = f (Grid arr d c' (b1, b2) bndrs)
>                                            in Reduce (mappend r r') ((c', x):as))
>              (Reduce r dats'') = foldl g (Reduce mempty []) (range (b1, b2))
>              arr' = array (b1, b2) dats''
>          in Reduce r (Grid arr' d c (b1, b2) NilB)
>

> run :: (IArray UArray y, Dimension d) => (Grid d b x -> y) -> Grid d b x -> Grid d Nil y
> run f (Grid arr d c (b1, b2) boundaries) =
>            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                arr' = array (b1, b2) dats'
>            in Grid arr' d c (b1, b2) NilB

> class RunGridA dyn where
>     runA :: (dyn ~ Dynamism b, IArray UArray a, Dimension d) => (Grid d b a -> a) -> Grid d b a -> Grid d b a

> instance RunGridA Dynamic where
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
