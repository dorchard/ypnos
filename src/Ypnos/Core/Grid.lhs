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

> module Ypnos.Core.Grid where

> import Ypnos.Core.Boundary
> import Ypnos.Core.Types
> import Ypnos.Core.Dimensions

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import Data.List

> import Debug.Trace

Grid data type

> data Grid d b dyn a where
>    Grid :: (UArray (Index d) a) ->                      -- Array of values
>            Dimensionality d ->                          -- Dimensionality term
>            Index d ->                                   -- Cursor ("current index") 
>            (Index d, Index d) ->                        -- Lower and upper bounds of extent
>            BoundaryList ixs dyn lower upper d a ->      -- Boundary information
>            Grid d ixs dyn a

> instance (Ix (Index d), IArray UArray a, 
>           Show (Index d), Show a) => Show (Grid d b dyn a) where
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
> instance InBoundary i (Cons i ixs)                    -- Head matches
> instance InBoundary i ixs => InBoundary i (Cons i' ixs)   -- Head does not match, thus recurse

Boundary lists, important for defining a grid

> data BoundaryList b dyn lower upper d a where
>     NilB :: BoundaryList Nil Static (Origin d) (Origin d) d a
>     ConsB :: BuildBoundary d ix dyn => 
>              BoundaryFun d ix a dyn
>               -> BoundaryList b dyn' lower upper d a 
>               -> BoundaryList (Cons (AbsToReln ix) b) (Dynamism dyn dyn') 
>                      (Lower (AbsToReln ix) lower) (Upper (AbsToReln ix) upper) d a

Boundaries functions

> data BoundaryFun d ix a dyn where
>     Static :: (ix -> a) -> BoundaryFun d ix a Static
>     Dynamic :: ((ix, (Grid d Nil Static a)) -> a) -> BoundaryFun d ix a Dynamic

"Plain" boundary lists (less type-level information, easier for manipulating)

> data BoundaryListP b d a where
>     NilBP :: BoundaryListP Nil d a 
>     ConsBP :: BoundaryFun d ix a dyn 
>               -> BoundaryListP b d a 
>               -> BoundaryListP (Cons (ix, dyn) b) d a

Computes the values of a boundary region, given a boundary list

> boundMap :: (IndexOps (Index d)) => Dimensionality d ->
>             BoundaryList ixs dyn lower upper d a -> Grid d Nil Static a ->
>             Index d -> Index d -> [(Index d, a)]
> boundMap d NilB _ _ _ = []
> boundMap d (ConsB f fs) g0 origin extent = (buildBoundary d f (origin, dec extent) g0) ++
>                                            boundMap d fs g0 origin extent


> getUpperIx :: (ReifiableIx upper b) => BoundaryList t dyn lower upper d a -> upper
> getUpperIx _ = typeToSymIx (undefined::upper)
> getLowerIx :: (ReifiableIx lower b) => BoundaryList t dyn lower upper d a -> lower
> getLowerIx _ = typeToSymIx (undefined::lower)

Generate boundary indices from boundary definitions

> class BuildBoundary d ix dyn where
>    buildBoundary :: Dimensionality d -> BoundaryFun d ix a dyn -> (Index d, Index d) ->
>                     (Grid d Nil Static a) -> [(Index d, a)]

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
>     bfun :: (Functor (Grid d Nil Static)) => 
>             BoundaryFun d ix a dyn -> BoundaryFun d ix b dyn -> BoundaryFun d ix (a, b) dyn 

> instance BFunZip Static where
>     bfun (Static x) (Static y) = Static (\i -> (x i, y i))

> instance BFunZip Dynamic where
>     bfun (Dynamic x) (Dynamic y) = Dynamic (\(i, g) -> (x (i, fmap fst g), y (i, fmap snd g)))

Boundary zipping

 bmatch [] [] = []
 bmatch (x:xs) (y:ys) | x == y    = (x, y) : (bmatch xs ys)
                      | otherwise = let (a, ys') = bmatch' x ys
                                    in a : (bmatch xs (y:ys'))

 bmatch' x [] = error "shouldn't happen"
 bmatch' x (z:zs) | (x == z) = ((x, z), zs)
                  | otherwise = let (a, zs') = bmatch' x zs
                                in (a, z : zs')

> class BMatch b b' rb d | b b' -> rb where
>     bmatch :: BoundaryListP b d x -> 
>               BoundaryListP b' d y ->
>               BoundaryListP rb d (x, y)

> instance BMatch Nil Nil Nil d where
>     bmatch NilBP NilBP = NilBP

> instance (Functor (Grid d Nil Static), BMatch xs ys zs d, BFunZip dyn) =>
>          BMatch (Cons (x, dyn) xs) (Cons (x, dyn) ys) (Cons (x, dyn) zs) d where
>     bmatch (ConsBP x xs) (ConsBP y ys) = ConsBP (bfun x y) (bmatch xs ys)
                                                            
> instance (BMatch' x dyn ys ys', BMatch xs (Cons y ys') zs d, BFunZip dyn, Functor (Grid d Nil Static)) =>
>          BMatch (Cons (x, dyn) xs) (Cons y ys) (Cons (x, dyn) zs) d where
>     bmatch (ConsBP x xs) (ConsBP y ys) =  let (y', ys') = bmatch' x ys
>                                           in ConsBP y' (bmatch xs (ConsBP y ys'))
                  

> class BMatch' ix dyn b rb | ix dyn b -> rb where
>     bmatch' :: (Functor (Grid d Nil Static), BFunZip dyn) => 
>                BoundaryFun d ix x dyn -> BoundaryListP b d y -> 
>                (BoundaryFun d ix (x, y) dyn, BoundaryListP rb d y)

> instance BMatch' ix dyn (Cons (ix, dyn) xs) xs where
>     bmatch' f (ConsBP x xs) = (bfun f x, xs)

> instance (BMatch' ix dyn ys ys') => BMatch' ix dyn (Cons y ys) (Cons y ys') where
>     bmatch' f (ConsBP y ys) = let (a, ys') = bmatch' f ys
>                               in (a, ConsBP y ys')
 
