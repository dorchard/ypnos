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

> import Ypnos.Core.Boundary
> import Ypnos.Core.Types
> import Ypnos.Core.Dimensions

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import GHC.Prim

> import Data.List

> import Debug.Trace

Grid data type

> data Grid d b a where
>    Grid :: (UArray (Index d) a) ->    -- Array of values
>            Dimensionality d ->        -- Dimensionality term
>            Index d ->                 -- Cursor ("current index")
>            (Index d, Index d) ->      -- Lower and upper bounds of extent
>            BoundaryList ixs d a ->     -- Boundary information
>            Grid d ixs a

> instance (Ix (Index d), IArray UArray a,
>           Show (Index d), Show a) => Show (Grid d b a) where
>     show (Grid arr d c (b1, b2) _) =
>           (show arr)++"@"++(show c)++" ["++(show b1)++", "++(show b2)++"]"

Constraints for enforcing safe indexing

> class Safe i b


1D Safety

> instance Safe (IntT (Pos Zn)) b

> instance (Safe (IntT (Pred n)) b,
>           InBoundary (IntT n) b) => Safe (IntT n) b

2D Safety

> instance Safe (IntT (Pos Zn), IntT (Pos Zn)) b

> instance (Safe (IntT (Pred n), IntT n') b,
>           Safe (IntT n, IntT (Pred n')) b,
>           InBoundary (IntT n, IntT n') b) => Safe (IntT n, IntT n') b

 3D Safety

> instance Safe (IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn)) b

> instance (Safe (IntT (Pred n), IntT n', IntT n'') b,
>           Safe (IntT n, IntT (Pred n'), IntT n'') b,
>           Safe (IntT n, IntT n', IntT (Pred n'')) b,
>           InBoundary (IntT n, IntT n', IntT n'') b) => Safe (IntT n, IntT n', IntT n'') b

> class InBoundary i ixs
> instance InBoundary i (Cons (i, dyn) ixs)                      -- Head matches
> instance InBoundary i ixs => InBoundary i (Cons (i', dyn) ixs) -- Head does not match, thus recurse

Boundary lists, important for defining a grid

> data BoundaryList b d a where
>     NilB :: BoundaryList Nil d a
>     ConsB :: (BuildBoundary d ix dyn, ReifiableIx ix (Index d)) =>
>               BoundaryFun d ix a dyn
>            -> BoundaryList b d a
>            -> BoundaryList (Cons (ix, dyn) b) d a

Boundaries functions

> data BoundaryFun d ix a dyn where
>     Static :: (ix -> a) -> BoundaryFun d ix a Static
>     Dynamic :: ((ix, (Grid d Nil a)) -> a) -> BoundaryFun d ix a Dynamic

Computes the values of a boundary region, given a boundary list

> boundMap :: (IndexOps (Index d)) => Dimensionality d ->
>             BoundaryList ixs d a -> Grid d Nil a ->
>             Index d -> Index d -> [(Index d, a)]
> boundMap d NilB _ _ _ = []
> boundMap d (ConsB f fs) g0 origin extent = (buildBoundary d f (origin, dec extent) g0) ++
>                                              boundMap d fs g0 origin extent

Generate boundary indices from boundary definitions

> class BuildBoundary d ix dyn where
>    buildBoundary :: Dimensionality d -> BoundaryFun d ix a dyn -> (Index d, Index d) ->
>                     (Grid d Nil a) -> [(Index d, a)]

> instance (ReifiableIx (IntT n) Int) => BuildBoundary (Dim d) (IntT n) Dynamic where
>     buildBoundary d (Dynamic f) (x0, xn) grid =
>         let
>             x = typeToIntIx (undefined::(IntT n))
>             x' = if (x>0) then (x+xn)
>                           else (x0+x)
>         in
>             [(x' , f (typeToSymIx (undefined::(IntT n)), grid))]

> instance (ReifiableIx (IntT n) Int) => BuildBoundary (Dim d) (IntT n) Static where
>     buildBoundary d (Static f) (x0, xn) grid =
>         let
>             x = typeToIntIx (undefined::(IntT n))
>             x' = if (x>0) then (x+xn)
>                           else (x0+x)
>         in
>             [(x' , f (typeToSymIx (undefined::(IntT n))))]

> instance (ReifiableIx (IntT n) Int, ReifiableIx (IntT m) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (IntT n, IntT m) Dynamic where
>     buildBoundary d (Dynamic f) ((x0, y0), (xn, yn)) grid =
>         let
>             x = typeToIntIx (undefined::(IntT n))
>             y = typeToIntIx (undefined::(IntT m))
>             x' = if (x>0) then (x+xn)
>                           else (x0+x)
>             y' = if (y>0) then (y+yn)
>                           else (y0+y)
>          in
>             [((x', y'), f ((typeToSymIx (undefined::(IntT n)), typeToSymIx (undefined::(IntT m))), grid))]


> instance (ReifiableIx (IntT n) Int, ReifiableIx (IntT m) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (IntT n, IntT m) Static where
>     buildBoundary d (Static f) ((x0, y0), (xn, yn)) grid =
>         let
>             x = typeToIntIx (undefined::(IntT n))
>             y = typeToIntIx (undefined::(IntT m))
>             x' = if (x>0) then (x+xn)
>                           else (x0+x)
>             y' = if (y>0) then (y+yn)
>                           else (y0+y)
>          in
>             [((x', y'), f (typeToSymIx (undefined::(IntT n)), typeToSymIx (undefined::(IntT m))))]

> instance (ReifiableIx (IntT n) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (IntT n, Int) Dynamic where
>     buildBoundary d (Dynamic f) ((x0, y0), (xn, yn)) grid =
>         let
>             x = typeToIntIx (undefined::(IntT n))
>             x' = if (x>0) then (x+xn)
>                           else (x0+x)
>         in
>             map (\y -> ((x', y),
>                 f ((typeToSymIx (undefined::(IntT n)), y), grid))) (range (y0, yn))

> instance (ReifiableIx (IntT n) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (IntT n, Int) Static where
>     buildBoundary d (Static f) ((x0, y0), (xn, yn)) grid =
>         let
>             x = typeToIntIx (undefined::(IntT n))
>             x' = if (x>0) then (x+xn)
>                           else (x0+x)
>         in
>             map (\y -> ((x', y),
>                 f (typeToSymIx (undefined::(IntT n)), y))) (range (y0, yn))

> instance (ReifiableIx (IntT m) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (Int, IntT m) Dynamic where
>     buildBoundary d (Dynamic f) ((x0, y0), (xn, yn)) grid =
>         let
>             y = typeToIntIx (undefined::(IntT m))
>             y' = if (y>0) then (y+yn)
>                           else (y0+y)
>         in
>             map (\x -> ((x, y'),
>                  f ((x, typeToSymIx (undefined::(IntT m))), grid))) (range (x0, xn))

> instance (ReifiableIx (IntT m) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (Int, IntT m) Static where
>     buildBoundary d (Static f) ((x0, y0), (xn, yn)) grid =
>         let
>             y = typeToIntIx (undefined::(IntT m))
>             y' = if (y>0) then (y+yn)
>                           else (y0+y)
>         in
>             map (\x -> ((x, y'),
>                  f (x, typeToSymIx (undefined::(IntT m))))) (range (x0, xn))

Zips together two boundary functions

> class BFunZip dyn where
>     bfunZip :: (Functor (Grid d Nil)) =>
>             BoundaryFun d ix a dyn -> BoundaryFun d ix b dyn -> BoundaryFun d ix (a, b) dyn
>     bfunUnzip :: (Functor (Grid d Nil)) =>
>             BoundaryFun d ix (a, b) dyn -> (BoundaryFun d ix a dyn, BoundaryFun d ix b dyn)

> instance BFunZip Static where
>     bfunZip (Static x) (Static y) = Static (\i -> (x i, y i))
>     bfunUnzip (Static x) = (Static (fst . x), Static (snd . x))

> instance BFunZip Dynamic where
>     bfunZip (Dynamic x) (Dynamic y) = Dynamic (\(i, g) -> (x (i, fmap fst g), y (i, fmap snd g)))
>     bfunUnzip (Dynamic x) = (Dynamic (\(i, g) -> fst $ x (i, fmap (\i -> (i, undefined)) g)),
>                              Dynamic (\(i, g) -> snd $ x (i, fmap (\i -> (undefined, i)) g)))

Boundary zipping

 bmatch [] [] = []
 bmatch (x:xs) (y:ys) | x == y    = (x, y) : (bmatch xs ys)
                      | otherwise = let (a, ys') = bmatch' x ys
                                    in a : (bmatch xs (y:ys'))

 bmatch' x [] = error "shouldn't happen"
 bmatch' x (z:zs) | (x == z) = ((x, z), zs)
                  | otherwise = let (a, zs') = bmatch' x zs
                                in (a, z : zs')

> class BUnzip b where
>     bUnzip :: (Functor (Grid d Nil)) => BoundaryList b d (x, y) -> (BoundaryList b d x, BoundaryList b d y)

> instance BUnzip Nil where
>     bUnzip NilB = (NilB, NilB)

> instance (BUnzip ixs, BFunZip dyn) => BUnzip (Cons (i, dyn) ixs) where
>     bUnzip (ConsB x xs) = let (y, z) = bUnzip xs
>                               (a, b) = bfunUnzip x
>                           in (ConsB a y, ConsB b z)

> class BZip b b' rb d | b b' -> rb where
>     bzip :: BoundaryList b d x ->
>             BoundaryList b' d y ->
>             BoundaryList rb d (x, y)
>

> instance BZip Nil Nil Nil d where
>     bzip NilB NilB = NilB
>

> instance (Functor (Grid d Nil), BZip xs ys zs d, BFunZip dyn) =>
>          BZip (Cons (x, dyn) xs) (Cons (x, dyn) ys) (Cons (x, dyn) zs) d where
>     bzip (ConsB x xs) (ConsB y ys) = ConsB (bfunZip x y) (bzip xs ys)
>

> instance (BZip' x dyn ys ys', BZip xs (Cons y ys') zs d, BFunZip dyn, Functor (Grid d Nil)) =>
>          BZip (Cons (x, dyn) xs) (Cons y ys) (Cons (x, dyn) zs) d where
>     bzip (ConsB x xs) (ConsB y ys) = let (y', ys') = bzip' x ys
>                                      in ConsB y' (bzip xs (ConsB y ys'))


> class BZip' ix dyn b rb | ix dyn b -> rb where
>     bzip' :: (Functor (Grid d Nil), BFunZip dyn) =>
>                BoundaryFun d ix x dyn -> BoundaryList b d y ->
>                (BoundaryFun d ix (x, y) dyn, BoundaryList rb d y)

> instance BZip' ix dyn (Cons (ix, dyn) xs) xs where
>     bzip' f (ConsB x xs) = (bfunZip f x, xs)

> instance (BZip' ix dyn ys ys') => BZip' ix dyn (Cons y ys) (Cons y ys') where
>     bzip' f (ConsB y ys) = let (a, ys') = bzip' f ys
>                            in (a, ConsB y ys')


Computes information on boundaries

> class (Dimension d) => BoundaryInfo ixs d where
>     lowerIx :: BoundaryList ixs d a -> Index d
>     upperIx :: BoundaryList ixs d a -> Index d

> instance (DimIdentifier d) => BoundaryInfo Nil (Dim d) where
>     lowerIx NilB = 0
>     upperIx NilB = 0

> instance (DimIdentifier d, ReifiableIx ix Int, BoundaryInfo ixs (Dim d))
>        => BoundaryInfo (Cons (ix, dyn) ixs) (Dim d) where
>     lowerIx (ConsB _ ixs) = min (lowerIx ixs) (typeToIntIx (undefined :: ix))
>     upperIx (ConsB _ ixs) = max (upperIx ixs) (typeToIntIx (undefined :: ix))

> instance (DimIdentifier d, DimIdentifier d') => BoundaryInfo Nil (Dim d :* Dim d') where
>     lowerIx NilB = (0, 0)
>     upperIx NilB = (0, 0)

> instance (DimIdentifier d, DimIdentifier d',
>           ReifiableIx ix (Int, Int), BoundaryInfo ixs (Dim d :* Dim d'))
>        => BoundaryInfo (Cons (ix, dyn) ixs) (Dim d :* Dim d') where
>     lowerIx (ConsB _ ixs) = let (x, y) = typeToIntIx (undefined :: ix)
>                                 (x', y') = lowerIx ixs
>                              in (x `min` x', y `min` y')
>     upperIx (ConsB _ ixs) = let (x, y) = typeToIntIx (undefined :: ix)
>                                 (x', y') = upperIx ixs
>                              in (x `max` x', y `max` y')
>
