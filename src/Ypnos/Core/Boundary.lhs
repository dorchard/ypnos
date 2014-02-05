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

> module Ypnos.Core.Boundary where

> import Ypnos.Core.Types
> import Ypnos.Core.Dimensions

 import Ypnos.Core.Grid

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import Data.List



Boundary lists, important for defining a grid

> data BoundaryList g b d a where
>     NilB :: BoundaryList g Nil d a
>     ConsB :: (BuildBoundary d ix dyn, ReifiableIx ix (Index d)) =>
>               BoundaryFun g d ix a dyn
>            -> BoundaryList g b d a
>            -> BoundaryList g (Cons (ix, dyn) b) d a

Boundaries functions

> data BoundaryFun (g :: * -> * -> * -> *) d ix a dyn where
>     Static :: (ix -> a) -> BoundaryFun g d ix a Static
>     Dynamic :: ((ix, (g d Nil a)) -> a) -> BoundaryFun g d ix a Dynamic

Computes the values of a boundary region, given a boundary list

> boundMap :: (IndexOps (Index d)) => Dimensionality d ->
>             BoundaryList g ixs d a -> g d Nil a ->
>             Index d -> Index d -> [(Index d, a)]
> boundMap d NilB _ _ _ = []
> boundMap d (ConsB f fs) g0 origin extent = (buildBoundary d f (origin, dec extent) g0) ++
>                                              boundMap d fs g0 origin extent

Generate boundary indices from boundary definitions

> class BuildBoundary d ix dyn where
>    buildBoundary :: Dimensionality d -> BoundaryFun g d ix a dyn -> (Index d, Index d) ->
>                     (g d Nil a) -> [(Index d, a)]

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
>     bfunZip :: (Functor (g d Nil)) =>
>             BoundaryFun g d ix a dyn -> BoundaryFun g d ix b dyn -> BoundaryFun g d ix (a, b) dyn
>     bfunUnzip :: (Functor (g d Nil)) =>
>             BoundaryFun g d ix (a, b) dyn -> (BoundaryFun g d ix a dyn, BoundaryFun g d ix b dyn)

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
>     bUnzip :: (Functor (g d Nil)) => BoundaryList g b d (x, y) -> (BoundaryList g b d x, BoundaryList g b d y)

> instance BUnzip Nil where
>     bUnzip NilB = (NilB, NilB)

> instance (BUnzip ixs, BFunZip dyn) => BUnzip (Cons (i, dyn) ixs) where
>     bUnzip (ConsB x xs) = let (y, z) = bUnzip xs
>                               (a, b) = bfunUnzip x
>                           in (ConsB a y, ConsB b z)

> class BZip b b' rb d | b b' -> rb where
>     bzip :: Functor (g d Nil) =>
>             BoundaryList g b d x ->
>             BoundaryList g b' d y ->
>             BoundaryList g rb d (x, y)

> instance BZip Nil Nil Nil d where
>     bzip NilB NilB = NilB

> instance (Functor (g d Nil), BZip xs ys zs d, BFunZip dyn) =>
>          BZip (Cons (x, dyn) xs) (Cons (x, dyn) ys) (Cons (x, dyn) zs) d where
>     bzip (ConsB x xs) (ConsB y ys) = ConsB (bfunZip x y) (bzip xs ys)

> instance (BZip' x dyn ys ys', BZip xs (Cons y ys') zs d, BFunZip dyn) =>
>          BZip (Cons (x, dyn) xs) (Cons y ys) (Cons (x, dyn) zs) d where
>     bzip (ConsB x xs) (ConsB y ys) = let (y', ys') = bzip' x ys
>                                      in ConsB y' (bzip xs (ConsB y ys'))

> class BZip' ix dyn b rb | ix dyn b -> rb where
>     bzip' :: (Functor (g d Nil), BFunZip dyn) =>
>                BoundaryFun g d ix x dyn -> BoundaryList g b d y ->
>                (BoundaryFun g d ix (x, y) dyn, BoundaryList g rb d y)

> instance BZip' ix dyn (Cons (ix, dyn) xs) xs where
>     bzip' f (ConsB x xs) = (bfunZip f x, xs)

> instance (BZip' ix dyn ys ys') => BZip' ix dyn (Cons y ys) (Cons y ys') where
>     bzip' f (ConsB y ys) = let (a, ys') = bzip' f ys
>                            in (a, ConsB y ys')


Computes information on boundaries

> class (Dimension d) => BoundaryInfo ixs d where
>     lowerIx :: BoundaryList g ixs d a -> Index d
>     upperIx :: BoundaryList g ixs d a -> Index d

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


Computes the dynamism of a boundary list

> type family DynamismP t t'
> type instance DynamismP Static Static = Static
> type instance DynamismP Static Dynamic = Dynamic
> type instance DynamismP Dynamic Static = Dynamic
> type instance DynamismP Dynamic Dynamic = Dynamic

> type family Dynamism t
> type instance Dynamism Nil = Static
> type instance Dynamism (Cons (ix, dyn) ixs) = DynamismP dyn (Dynamism ixs)


 type family Append l l'
 type instance Append Nil ys = ys
 type instance Append (Cons x xs) ys = Cons x (Append xs ys)

 appendBP :: BoundaryListP b d a -> BoundaryListP b' d a -> BoundaryListP (Append b b') d a
 appendBP NilB y = y
 appendBP (ConsBP x xs) ys = ConsBP x (appendBP xs ys)



