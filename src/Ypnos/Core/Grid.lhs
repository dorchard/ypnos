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

> {-# LANGUAGE UndecidableInstances #-}

> module Ypnos.Core.Grid where

> -- import Ypnos.Core.Boundary

> import Ypnos.Core.Combinators
> import Ypnos.Core.Types
> import Ypnos.Core.Dimensions

> import Ypnos.Core.Boundary


> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import GHC.Prim

> import Data.Monoid
> import Data.List

> import Debug.Trace

General grid interface

> class GridConstructor (g :: * -> * ->  * -> *) where

>   type ConstInv g d b a :: Constraint

>   listGrid :: (ConstInv g d b a, Dimension d) => 
>                  Dimensionality d
>                -> (Index d, Index d)
>                -> [a] 
>                -> BoundaryList g b d a
>                -> g d b a

   type DataConst g d b a :: Constraint

   gridData :: (DataConst g d b a, Dimension d) =>
               g d b a -> [a]

>   -- Bounds-checked indexing
>   type IxConst g d b a :: Constraint
>   (!!!) :: IxConst g d b a => g d b a -> Index d -> a


Specific core "Grid" type for Ypnos

> data Grid d b a where
>    Grid :: (UArray (Index d) a) ->    -- Array of values
>            Dimensionality d ->        -- Dimensionality term
>            Index d ->                 -- Cursor ("current index")
>            (Index d, Index d) ->      -- Lower and upper bounds of extent
>            BoundaryList Grid ixs d a ->     -- Boundary information
>            Grid d ixs a

> instance (Ix (Index d), IArray UArray a,
>           Show (Index d), Show a) => Show (Grid d b a) where
>     show (Grid arr d c (b1, b2) _) =
>           (show arr)++"@"++(show c)++" ["++(show b1)++", "++(show b2)++"]"

> type instance ElemInv (Grid d) x = IArray UArray x

> instance GridConstructor Grid where

>   type ConstInv Grid d b a = (IArray UArray a, BoundaryInfo b d)
>   listGrid = listGrid'

   type DataConst (Grid d b) d b a => (PointwiseOrd (Index d), IArray UArray a)
   gridData = gridData'

>   type IxConst Grid d b a = (IArray UArray a, Ix (Index d))
>   (Grid arr _ _ _ _) !!! i = arr!i 


Grid run

General gather over a grid. Note, this destroys the boundaries

> runG :: (IArray UArray y, Dimension d) => (Grid d b x -> y) -> Grid d b x -> Grid d Nil y
> runG f (Grid arr d c (b1, b2) boundaries) =
>            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                arr' = array (b1, b2) dats'
>            in Grid arr' d c (b1, b2) NilB

Gather over a grid, preserving boundaries

> class RunGridA dyn where
>     runGA :: (dyn ~ Dynamism b, IArray UArray a, Dimension d) => 
>              (Grid d b a -> a) -> Grid d b a -> Grid d b a

> instance RunGridA Dynamic where
>     runGA f (Grid arr d c (b1, b2) boundaries) = 
>             let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                 arr' = accum (curry snd) arr dats'
>                 g0 = Grid arr' d c (b1, b2) NilB
>                 es = boundMap d boundaries g0 b1 b2
>                 (b1', b2') = bounds arr
>                 arr'' = array (b1', b2') (dats'++es)
>             in
>                 Grid arr'' d c (b1, b2) boundaries


> instance RunGridA Static where
>     runGA f (Grid arr d c (b1, b2) boundaries) = 
>             let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                 arr' = accum (curry snd) arr dats'
>             in  Grid arr' d c (b1, b2) boundaries


> runGReduceSimple :: (IArray UArray y, Dimension d, Monoid r) =>
>                     (Grid d b x -> (r, y))
>                  -> Grid d b x
>                  -> (r, Grid d Nil y)
> runGReduceSimple f (Grid arr d c (b1, b2) bndrs) = 
>          let g = (\(r, as) -> \c' -> let (r', x) = f (Grid arr d c' (b1, b2) bndrs)
>                                      in  (mappend r r', ((c', x):as)))
>              (r, dats'') = foldl g (mempty, []) (range (b1, b2))
>              arr' = array (b1, b2) dats''
>           in (r, Grid arr' d c (b1, b2) NilB)




Grid indexing 


The following is desugared from !!! inside a boundary macro

> ypnosReservedBoundaryIndex :: (IArray UArray a, Dimension d) => Grid d Nil a -> Index d -> a
> ypnosReservedBoundaryIndex (Grid arr _ _ _ _) i = arr!i

Hidden by grid patterns

> indexC :: (Dimension d, IArray UArray a) => Grid d b  a -> a
> indexC (Grid arr _ c _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) c)

> {-# INLINE index1D #-}
> index1D :: (Safe (IntT n) (Absify b), IArray UArray a) => IntT n -> Int -> Grid (Dim d) b a -> a
> index1D _ n (Grid arr d x _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n))

> {-# INLINE index2D #-}
> index2D :: (Safe (IntT n, IntT n') (Absify b), IArray UArray a) => (IntT n, IntT n') -> (Int, Int) -> Grid (Dim d :* Dim d') b a -> a
> index2D _ (n, n') (Grid arr d (x, y) _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n, y + n'))

> {-# INLINE index3D #-}
> index3D :: (Safe (IntT n, IntT n', IntT n'') (Absify b), IArray UArray a) => (IntT n, IntT n', IntT n'') -> (Int, Int, Int) -> Grid (Dim d :* (Dim d' :* Dim d'')) b a -> a
> index3D _ (n, n', n'') (Grid arr _ (x, y, z) _ _) = arr!(x + n, y + n', z + n'')

 index3D (n, n', n'') (Grid arr _ (x, y, z) _ _) = arr!(x + intTtoInt n, y + intTtoInt n', z + intTtoInt n'')

> instance Indexing (Grid (Dim d)) (IntT n) where
>     type IndexG (Grid (Dim d)) (IntT n) = (IntT n, Int)
>     index = uncurry index1D

> instance Indexing (Grid (Dim d :* Dim d')) (IntT n, IntT n') where
>     type IndexG (Grid (Dim d :* Dim d')) (IntT n, IntT n') = ((IntT n, IntT n'), (Int, Int))
>     index = uncurry index2D

> instance Indexing (Grid (Dim d :* (Dim d' :* Dim d''))) (IntT n, IntT n', IntT n'') where
>     type IndexG (Grid (Dim d :* (Dim d' :* Dim d''))) (IntT n, IntT n', IntT n'')
>               = ((IntT n, IntT n', IntT n''), (Int, Int, Int))
>     index = uncurry index3D

 {-# INLINE unsafeIndex2D #-}
 unsafeIndex2D :: (IArray UArray a) => (Int, Int) -> Grid (Dim d :* Dim d') b a -> a
 unsafeIndex2D (n, n') (Grid arr d (x, y) _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n, y + n'))

 {-# INLINE unsafeIndex1D #-}
 unsafeIndex1D :: (IArray UArray a) => Int -> Grid (Dim d) b a -> a
 unsafeIndex1D n (Grid arr d x _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n))

Standard constructor from a list of index-value pairs

> grid :: (BoundaryInfo ixs d, IArray UArray a, Dimension d) =>
>         Dimensionality d -> Index d -> Index d -> [(Index d, a)] ->
>         BoundaryList Grid ixs d a ->
>         Grid d ixs a

> grid d origin extent xs boundaries =
>              Grid arr d origin (origin, (dec extent)) boundaries
>              where
>                 g0 = gridNoBoundary d origin extent xs
>                 es = boundMap d boundaries g0 origin extent
>                 origin' = add origin (lowerIx boundaries)
>                 extent' = add extent (upperIx boundaries)
>                 arr = array (origin', (dec extent')) (es++xs)

Construct a grid from a list of just values

> listGrid' :: (BoundaryInfo ixs d, IArray UArray a, Dimension d) =>
>                  Dimensionality d -> (Index d, Index d) -> [a] ->
>                  BoundaryList Grid ixs d a ->
>                  Grid d ixs a
> listGrid' d (origin, extent) xs boundaries =
>   Grid arr d origin (origin, (dec extent)) boundaries
>              where
>                 g0 = listGridNoBoundary d origin extent xs
>                 es = boundMap d boundaries g0 origin extent
>                 origin' = add origin (lowerIx boundaries)
>                 extent' = add extent (upperIx boundaries)
>                 xs' = zip (range (origin, (dec extent))) xs
>                 -- xs' = zip (map invert (range (invert $ origin, invert $ (dec extent)))) xs
>
>                 arr = array (origin', dec extent') (es++xs')

Grid constructors

> listGridNoBoundary :: (IArray UArray a, Dimension d) => 
>                       Dimensionality d -> Index d -> Index d -> [a] -> Grid d Nil a
> listGridNoBoundary d origin extent xs =
>            let arr = listArray (origin, (dec extent)) xs
>            in  Grid arr d origin (origin, (dec extent)) NilB

> gridNoBoundary :: (IArray UArray a, Dimension d) =>
>                   Dimensionality d -> Index d -> Index d -> [(Index d, a)] -> Grid d Nil a
> gridNoBoundary d origin extent xs =
>            let arr = array (origin, (dec extent)) xs
>            in  Grid arr d origin (origin, (dec extent)) NilB


Deconstructors 

> instance (Dimension d, PointwiseOrd (Index d)) => Data (Grid d) where
>     type DataInv (Grid d) a = (IArray UArray a)
>     getData (Grid arr _ _ (origin, extent) _) =
>              let -- invert' = \(i, a) -> (invert i, a)
>                  -- xs = map invert' (sortBy (\(i, _) -> \(i', _) -> compare i i') (map invert' $ assocs arr))
>                  xs = sortBy (\(i, _) -> \(i', _) -> compare i i') (assocs arr)
>                  xs' = filter (\(i, a) -> gte i origin && lte i extent) xs
>              in  map snd xs'


> type instance SizeAbs (g ((Dim d) :* (Dim d'))) = (Index (Dim d), Index (Dim d'))
> type instance SizeAbs (g (Dim d)) = (Index (Dim d))

> instance (DimIdentifier d) => Size (Grid (Dim d)) where
>     size (Grid _ _ _ (l, b) _) = b - l

> instance (DimIdentifier d, DimIdentifier d') => Size (Grid ((Dim d) :* (Dim d'))) where
>     size (Grid _ _ _ ((lx,ly), (ux,uy)) _) = (ux-lx, uy-ly)

Ziping and unzipping grids

> instance (Functor (Grid d Nil), Dimension d) => Zip (Grid d) where
>     type BZipC (Grid d) b b' b'' = BZip b b' b'' d
>     type BUnzipC (Grid d) b      = (BUnzip b)

>     zipC = gridZip
>     unzipC = gridUnzip

> gridZip :: (IArray UArray x, IArray UArray y, IArray UArray (x, y),
>             BZip b b' b'' d, Functor (Grid d Nil), 
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


