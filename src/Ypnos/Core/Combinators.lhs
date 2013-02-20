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
> {-# LANGUAGE UndecidableInstances #-}

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

> import Data.List

> import Debug.Trace

Ypnos classes.


> class RunGrid grid sh where
>     data Sten sh a b
>     runG :: Sten sh a b -> grid a -> grid b
> 
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

> -- Indexing

The following is desugared from !!! inside a boundary macro

> ypnosReservedBoundaryIndex :: (IArray UArray a, Dimension d) => Grid d Nil Static a -> Index d -> a
> ypnosReservedBoundaryIndex (Grid arr _ _ _ _) i = arr!i

Hidden by grid patterns

> {-# INLINE index1D #-}
> index1D :: (Safe (IntT n) b, IArray UArray a) => IntT n -> Int -> Grid (Dim d) b dyn a -> a
> index1D _ n (Grid arr d x _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n))

OLD form

 index1D :: (Safe (IntT n) b, IArray UArray a) => IntT n -> Grid (Dim d) b dyn a -> a
 index1D n (Grid arr _ c _ _) = arr!(c + (intTtoInt n))

> {-# INLINE index2D #-}
> index2D :: (Safe (IntT n, IntT n') b, IArray UArray a) => (IntT n, IntT n') -> (Int, Int) -> Grid (Dim d :* Dim d') b dyn a -> a
> index2D _ (n, n') (Grid arr d (x, y) _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n, y + n'))

OLD form

 index2D :: (Safe (IntT n, IntT n') b, IArray UArray a) => (IntT n, IntT n') -> Grid (Dim d :* Dim d') b dyn a -> a
 index2D (n, n') (Grid arr d (x, y) _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + intTtoInt n, y + intTtoInt n'))

> {-# INLINE index3D #-}
> index3D :: (Safe (IntT n, IntT n', IntT n'') b, IArray UArray a) => (IntT n, IntT n', IntT n'') -> Grid (Dim d :* (Dim d' :* Dim d'')) b dyn a -> a
> index3D (n, n', n'') (Grid arr _ (x, y, z) _ _) = arr!(x + intTtoInt n, y + intTtoInt n', z + intTtoInt n'')

> {-# INLINE unsafeIndex2D #-}
> unsafeIndex2D :: (IArray UArray a) => (Int, Int) -> Grid (Dim d :* Dim d') b dyn a -> a
> unsafeIndex2D (n, n') (Grid arr d (x, y) _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n, y + n'))

> {-# INLINE unsafeIndex1D #-}
> unsafeIndex1D :: (IArray UArray a) => Int -> Grid (Dim d) b dyn a -> a
> unsafeIndex1D n (Grid arr d x _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n))

 class RelativeIndex i d where
     index :: (Safe i b, IArray UArray a) => i -> Grid d b dyn a -> a
 instance RelativeIndex (IntT n) (Dim d) where
     index = index1D
 instance RelativeIndex (IntT n, IntT n') (Dim d :* Dim d') where
     index = index2D
 instance RelativeIndex (IntT n, IntT n', IntT n'') (Dim d :* (Dim d' :* Dim d'')) where
     index = index3D

> indexC :: (Dimension d, IArray UArray a) => Grid d b dyn a -> a
> indexC (Grid arr _ c _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) c)

> (!!!) :: (IArray UArray a, Ix (Index d)) => Grid d b dyn a -> Index d -> a
> (Grid arr _ _ _ _) !!! i = arr!i

> ix :: (IArray UArray a, Ix (Index d)) => Grid d b dyn a -> Index d -> a
> ix = (!!!)

> -- Deconstructor

> gridData :: (Dimension d, PointwiseOrd (Index d), IArray UArray a) => Grid d b dyn a -> [a]
> gridData (Grid arr _ _ (origin, extent) _) = 
>             let invert' = \(i, a) -> (invert i, a)
>                 xs = map invert' (sortBy (\(i, _) -> \(i', _) -> compare i i') (map invert' $ assocs arr))  
>                 xs' = filter (\(i, a) -> gte i origin && lte i extent) xs
>             in 
>               map snd xs'

> class (Dimension d) => Size d where
>     size :: Grid d b i a -> (Index d)

> instance (DimIdentifier d) => Size (Dim d) where
>     size (Grid _ _ _ (l, b) _) = b - l

> instance (DimIdentifier d, DimIdentifier d') => Size ((Dim d) :* (Dim d')) where
>     size (Grid _ _ _ ((lx,ly), (ux,uy)) _) = (ux-lx, uy-ly)


> -- Constructors

> listGridNoBoundary :: (IArray UArray a, Dimension d) => Dimensionality d -> Index d -> Index d -> [a] -> Grid d Nil Static a
> listGridNoBoundary d origin extent xs = Grid arr d origin (origin, (dec extent)) NilB
>                                  where arr = listArray (origin, (dec extent)) xs

> gridNoBoundary :: (IArray UArray a, Dimension d) => Dimensionality d -> Index d -> Index d -> [(Index d, a)] -> Grid d Nil Static a
> gridNoBoundary d origin extent xs = Grid arr d origin (origin, (dec extent)) NilB
>                              where arr = array (origin, (dec extent)) xs



> grid :: (IArray UArray a, Dimension d, 
>                  ReifiableIx upper (Index d),
>                  ReifiableIx lower (Index d)) =>
>                 Dimensionality d -> Index d -> Index d -> [(Index d, a)] ->
>                 BoundaryList b dyn lower upper d a ->
>                 Grid d b dyn a

> grid d origin extent xs boundaries =
>              Grid arr d origin (origin, (dec extent)) boundaries
>              where 
>                 g0 = gridNoBoundary d origin extent xs
>                 es = boundMap d boundaries g0 origin extent 
>                 origin' = add origin (typeToIntIx $ getLowerIx boundaries)
>                 extent' = add extent (typeToIntIx $ getUpperIx boundaries)
>                 arr = array (origin', (dec extent')) (es++xs)

> listGrid :: (IArray UArray a, Dimension d,
>                  ReifiableIx upper (Index d),
>                  ReifiableIx lower (Index d)) =>
>                  Dimensionality d -> Index d -> Index d -> [a] ->
>                  BoundaryList b dyn lower upper d a ->
>                  Grid d b dyn a
> listGrid d origin extent xs boundaries =
>              Grid arr d origin (origin, (dec extent)) boundaries
>              where 
>                 g0 = listGridNoBoundary d origin extent xs
>                 es = boundMap d boundaries g0 origin extent 
>                 origin' = add origin (typeToIntIx $ getLowerIx boundaries)
>                 extent' = add extent (typeToIntIx $ getUpperIx boundaries)
>                 xs' = zip (map invert (range (invert $ origin, invert $ (dec extent)))) xs
>                 arr = array (origin', dec extent') (es++xs')

> gridZip :: (IArray UArray x, IArray UArray y, IArray UArray (x, y), Dimension d) => 
>            Grid d b dyn x -> Grid d b dyn y -> Grid d b dyn (x, y)
> gridZip (Grid arr d c (l, u) b) (Grid arr' _ c' (l', u') b') 
>     | (l /= l') || (u /= u') = error "Can only zip grids of the same size"
>     | c /= c'                = error "Can only zip grids with the same cursor"
>     | otherwise              = let arr'' = array (l, u) (map (\((i,x),(i',y)) -> (i, (x, y))) (zip (assocs arr) (assocs arr')))
>                                    b'' = undefined
>                                in (Grid arr'' d c (l, u) b'')


 boundaryZip NilB NilB = NilB
 boundaryZip :: BoundaryList b dyn lower upper d x -> BoundaryList b' dyn lower upper d y ->
                BoundaryList b dyn lower upper d (x, y)


> -- Run stencil computations

> run :: (IArray UArray y, Dimension d) => (Grid d b dyn x -> y) -> Grid d b dyn x -> Grid d Nil Static y
> run f (Grid arr d c (b1, b2) boundaries) =
>            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                arr' = array (b1, b2) dats'
>            in Grid arr' d c (b1, b2) NilB

> class RunGridA dyn where
>     runA :: (IArray UArray a, Dimension d) => (Grid d b dyn a -> a) -> Grid d b dyn a -> Grid d b dyn a

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

