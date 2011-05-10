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

> module Ypnos.Core.Grid where

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import Data.List

> import Debug.Trace

> -- Type level integers

> data Zn
> data S n 
> data Neg n

> data Nat n where
>     Zn :: Nat Zn
>     S :: Nat n -> Nat (S n)
>     Neg :: Nat n -> Nat (Neg n)

> natToInt :: Nat n -> Int
> natToInt Zn = 0
> natToInt (S n) = 1 + natToInt n
> natToInt (Neg n) = - natToInt n

> -- Some helpers, synonyms for numbers

> instance Show (Nat Zn) where
>     show Zn = "Z"
> instance Show (Nat n) => Show (Nat (S n)) where
>     show (S n) = "S" ++ (show n)
> instance Show (Nat n) => Show (Nat (Neg n)) where
>     show (Neg n) = "-" ++ (show n)

> type Zero = Zn
> type One = S Zn
> type Two = S (S Zn)
> type Three = S (S (S Zn))
> type Four = S (S (S (S Zn)))
> type Five = S (S (S (S (S Zn))))
> type Six = S (S (S (S (S (S Zn)))))
> type Seven = S (S (S (S (S (S (S Zn))))))

> data family NatRepr t
> data instance NatRepr Zn = Zero
> data instance NatRepr (S Zn) = One
> data instance NatRepr (S (S Zn)) = Two
> data instance NatRepr (S (S (S Zn))) = Three
> data instance NatRepr (S (S (S (S Zn)))) = Four
> data instance NatRepr (S (S (S (S (S Zn))))) = Five
> data instance NatRepr (S (S (S (S (S (S Zn)))))) = Six
> data instance NatRepr (S (S (S (S (S (S (S Zn))))))) = Seven

> -- Type-level list (hetro-geneous list)

> data Nil
> data Cons a b

> type (:::) = Cons
> 
> data List l where
>     Nil :: List Nil
>     Cons :: n -> List m -> List (Cons n m)

> -- Dimensions

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

> data Dim d 
> data (:*) d d'

> data Dimensionality d where
>     Dim :: (Dimension (Dim d), DimIdentifier d) => d -> Dimensionality (Dim d)
>     (:*) :: (Dimension (Dim d :* d')) => Dimensionality (Dim d) -> Dimensionality d' -> Dimensionality (Dim d :* d')

> -- Indices terms

> type family Index t
> type instance Index () = ()
> type instance Index (Dim d) = Int
> type instance Index ((Dim d) :* (Dim d')) = (Int, Int)
> type instance Index ((Dim d) :* ((Dim d') :* (Dim d''))) = (Int, Int, Int)
> type instance Index ((Dim x) :* ((Dim y) :* ((Dim z) :* (Dim w)))) = (Int, Int, Int, Int)
> type instance Index ((Dim x) :* ((Dim y) :* ((Dim z) :* ((Dim w) :* (Dim u))))) = (Int, Int, Int, Int, Int)

> -- Grid data type

> data Grid d b a where
>     Grid :: (UArray (Index d) a) -> Dimensionality d -> Index d -> (Index d, Index d) ->
>              BoundaryList ixs dyn lower upper d a -> 
>              Grid d (SafeRelativeIndices ixs, dyn) a

> instance (Ix (Index d), IArray UArray a, 
>           Show (Index d), Show a) => Show (Grid d b a) where
>     show (Grid arr d c (b1, b2) _) = (show arr)++"@"++(show c)++" ["++(show b1)++", "++(show b2)++"]"

> -- Safe relative indexes

> type family InnerToZn t
> type instance InnerToZn Int = Nat Zn
> type instance InnerToZn (Nat n) = Nat n
> type instance InnerToZn (a, b) = (InnerToZn a, InnerToZn b)
> type instance InnerToZn (a, b, c) = (InnerToZn a, InnerToZn b, InnerToZn c)

> type family SafeRelativeIndices t
> type instance SafeRelativeIndices Nil = Nil
> type instance SafeRelativeIndices (Cons x xs) = Cons (InnerToZn x) (SafeRelativeIndices xs)

> -- Boundaries functions and list tpes

> data Static 
> data Dynamic g

> data BoundaryFun ix a t where
>     Static :: (ix -> a) -> BoundaryFun ix a Static
>     Dynamic :: ((ix, g) -> a) -> BoundaryFun ix a (Dynamic g)

> data BoundaryList t dyn lower upper d a where
>     NilB :: BoundaryList Nil Static () () d a
>     ConsB :: BuildBoundary d ix dyn (Grid d (Nil, Static) a) => 
>               BoundaryFun ix a dyn
>               -> BoundaryList t dyn' lower upper d a 
>               -> BoundaryList (Cons ix t) (Dynamism dyn dyn') (Lower ix lower) (Upper ix upper) d a

> -- Type functions for combining inductively defined boundary info

> type family Dynamism t t'
> type instance Dynamism Static Static = Static
> type instance Dynamism Static (Dynamic g) = Dynamic g
> type instance Dynamism (Dynamic g) Static = Dynamic g
> type instance Dynamism (Dynamic g) (Dynamic g) = Dynamic g
     
> type family Upper t t'
> type instance Upper (Neg n) () = Zn
> type instance Upper (S n) () = S n
> type instance Upper Zn () = Zn
> type instance Upper Int () = Nat Zn
> type instance Upper (Nat n) () = Nat (Upper n ())
> type instance Upper (a, b) () = (Upper a (), Upper b ())
> type instance Upper (a, b, c) () = (Upper a (), Upper b (), Upper c ())

> type instance Upper Zn Zn = Zn
> type instance Upper Zn (S n) = S n
> type instance Upper Zn (Neg n) = Zn
> type instance Upper Zn Int = Zn
> type instance Upper (S n) Zn = S n
> type instance Upper (S n) (S m) = S (Upper n m)
> type instance Upper (S n) (Neg m) = S n
> type instance Upper (S n) Int = S n
> type instance Upper (Neg n) Zn = Zn
> type instance Upper (Neg n) (S m) = S m
> type instance Upper (Neg n) (Neg m) = Neg (Lower n m)
> type instance Upper (Neg n) Int = Neg n
> type instance Upper Int Zn = Int
> type instance Upper Int (S n) = S n
> type instance Upper Int (Neg n) = Int
> type instance Upper Int Int = Int

> type instance Upper (Nat a) Int = Nat a
> type instance Upper Int (Nat a) = Nat a
> type instance Upper (Nat a) (Nat b) = Nat (Upper a b)
> type instance Upper (a, b) (c, d) = (Upper a c, Upper b d)
> type instance Upper (a, b, c) (d, e, f) = (Upper a d, Upper b e, Upper c f)
> type instance Upper (a, b, c, d) (e, f, g, h) = (Upper a e, Upper b f, Upper c g, Upper d h)

> type family Lower t t'
> type instance Lower (Neg n) () = Neg n
> type instance Lower (S n) () = Zn
> type instance Lower Zn () = Zn
> type instance Lower Int () = Nat Zn
> type instance Lower (Nat n) () = Nat (Lower n ())
> type instance Lower (a, b) () = (Lower a (), Lower b ())
> type instance Lower (a, b, c) () = (Lower a (), Lower b (), Lower c ())

> type instance Lower Zn Zn = Zn
> type instance Lower Zn (S n) = Zn
> type instance Lower Zn (Neg n) = Neg n
> type instance Lower Zn Int = Zn
> type instance Lower (S n) Zn = Zn
> type instance Lower (S n) (S m) = S (Lower n m)
> type instance Lower (S n) (Neg m) = Neg m
> type instance Lower (S n) Int = S n
> type instance Lower (Neg n) Zn = Neg n
> type instance Lower (Neg n) (S m) = Neg n
> type instance Lower (Neg n) (Neg m) = Neg (Upper n m)
> type instance Lower (Neg n) Int = Neg n
> type instance Lower Int Zn = Int
> type instance Lower Int (S n) = Int
> type instance Lower Int (Neg n) = Neg n
> type instance Lower Int Int = Int

> type instance Lower (Nat a) (Nat b) = Nat (Lower a b)
> type instance Lower Int (Nat a) = Nat a
> type instance Lower (Nat a) Int = Nat a

> type instance Lower (a, b) (c, d) = (Lower a c, Lower b d)
> type instance Lower (a, b, c) (d, e, f) = (Lower a d, Lower b e, Lower c f)
> type instance Lower (a, b, c, d) (e, f, g, h) = (Lower a e, Lower b f, Lower c g, Lower d h)

> -- Enforces safe indexing

> class InBoundary n b
> instance InBoundary n (Cons n y)
> instance InBoundary n y => InBoundary n (Cons n' y)

> -- A zero relative index is always within the boundary
> instance InBoundary (Nat Zn) Nil
> instance InBoundary (Nat Zn, Nat Zn) Nil
> instance InBoundary (Nat Zn, Nat Zn, Nat Zn) Nil
> instance InBoundary (Nat Zn, Nat Zn, Nat Zn, Nat Zn) Nil
> instance InBoundary (Nat Zn, Nat Zn, Nat Zn, Nat Zn, Nat Zn) Nil

> -- Computes the values of a boundary region, given a boundary list

> boundMap :: (IndexOps (Index d)) => Dimensionality d ->
>             BoundaryList ixs dyn lower upper d a -> Grid d (Nil, Static) a ->
>             Index d -> Index d -> [(Index d, a)]
> boundMap d NilB _ _ _ = []
> boundMap d (ConsB f fs) g0 origin extent = (buildBoundary d f (origin, dec extent) g0) ++
>                                            boundMap d fs g0 origin extent




> -- Reify nat types as nat data and int data

> getUpperIx :: (ReifiableIx upper b) => BoundaryList t dyn lower upper d a -> upper
> getUpperIx _ = typeToSymIx (undefined::upper)
> getLowerIx :: (ReifiableIx lower b) => BoundaryList t dyn lower upper d a -> lower
> getLowerIx _ = typeToSymIx (undefined::lower)

> class ReifiableIx t t' | t -> t' where
>     typeToIntIx :: t -> t'
>     typeToSymIx :: t -> t

 instance ReifiableIx (Nat Int) Int where
     typeToIntIx = undefined

> instance ReifiableIx (Nat Zn) Int where
>     typeToIntIx _ = 0
>     typeToSymIx _ = Zn 

> instance ReifiableIx (Nat n) Int => ReifiableIx (Nat (S n)) Int where
>     typeToIntIx _ = 1 + (typeToIntIx (undefined::(Nat n)))
>     typeToSymIx _ = S (typeToSymIx (undefined::(Nat n)))

> instance ReifiableIx (Nat n) Int => ReifiableIx (Nat (Neg n)) Int where
>     typeToIntIx _ = - (typeToIntIx (undefined::(Nat n)))
>     typeToSymIx _ = Neg (typeToSymIx (undefined::(Nat n)))

> instance (ReifiableIx (Nat a) Int, ReifiableIx (Nat b) Int) => 
>          ReifiableIx (Nat a, Nat b) (Int, Int) where
>     typeToIntIx _ = (typeToIntIx (undefined::(Nat a)), typeToIntIx (undefined::(Nat b)))
>     typeToSymIx _ = (typeToSymIx (undefined::(Nat a)), typeToSymIx (undefined::(Nat b)))

> instance (ReifiableIx (Nat a) Int, ReifiableIx (Nat b) Int, ReifiableIx (Nat c) Int) =>
>          ReifiableIx (Nat a, Nat b, Nat c) (Int, Int, Int) where
>     typeToIntIx _ = (typeToIntIx (undefined::(Nat a)), typeToIntIx (undefined::(Nat b)), typeToIntIx (undefined::(Nat c)))
>     typeToSymIx _ = (typeToSymIx (undefined::(Nat a)), typeToSymIx (undefined::(Nat b)),
>                                typeToSymIx (undefined::(Nat c)))


> -- Various operations on indices

> -- some Num functionality
> class IndexOps ix where
>     dec :: ix -> ix                  -- decrement an index
>     add :: ix -> ix -> ix            -- add two indices
>     invert :: ix -> ix               -- invert the index
>     sortR :: [(ix, a)] -> [(ix, a)]  -- radix sort the index (from left->right)
> instance IndexOps Int where
>     dec x = x - 1
>     add x y = x + y
>     invert = id
>     sortR = sortBy (\(i, a) -> \(j, b) -> compare i j)
> instance IndexOps (Int, Int) where
>     dec (x, y) = (x - 1, y - 1)
>     add (x, a) (y, b) = (x+y, a+b)
>     invert (x, y) = (y, x)
>     sortR = (sortBy (\((_, i), a) -> \((_, j), b) -> compare i j)) .
>             (sortBy (\((i, _), a) -> \((j, _), b) -> compare i j))
> instance IndexOps (Int, Int, Int) where
>     dec (x, y, z) = (x -1, y - 1, z - 1)
>     add (x, a, u) (y, b, v) = (x+y, a+b, u+v)
>     invert (x, y, z) = (z, y, x)
>     sortR = (sortBy (\((_, _, i), a) -> \((_, _, j), b) -> compare i j)) .
>             (sortBy (\((_, i, _), a) -> \((_, j, _), b) -> compare i j)) .
>             (sortBy (\((i, _, _), a) -> \((j, _, _), b) -> compare i j))

> instance IndexOps (Int, Int, Int, Int) where
>     dec (x, y, z, w) = (x - 1, y - 1, z - 1, w - 1)
>     add (x, a, u, f) (y, b, v, g) = (x+y, a+b, u+v, f+g)
>     invert (x, y, z, w) = (w, z, y, x)
>     sortR = (sortBy (\((_, _, _, i), a) -> \((_, _, _, j), b) -> compare i j)) .
>             (sortBy (\((_, _, i, _), a) -> \((_, _, j, _), b) -> compare i j)) .
>             (sortBy (\((_, i, _, _), a) -> \((_, j, _, _), b) -> compare i j)) .
>             (sortBy (\((i, _, _, _), a) -> \((j, _, _, _), b) -> compare i j))

> instance IndexOps (Int, Int, Int, Int, Int) where
>     dec (x, y, z, w, a) = (x - 1, y - 1, z - 1, w - 1, a - 1)
>     add (x, a, u, f, i) (y, b, v, g, j) = (x+y, a+b, u+v, f+g, i+j)
>     invert (x, y, z, w, a) = (a, w, z, y, x)
>     sortR = (sortBy (\((_, _, _, _, i), a) -> \((_, _, _, _, j), b) -> compare i j)) .
>             (sortBy (\((_, _, _, i, _), a) -> \((_, _, _, j, _), b) -> compare i j)) .
>             (sortBy (\((_, _, i, _, _), a) -> \((_, _, j, _, _), b) -> compare i j)) .
>             (sortBy (\((_, i, _, _, _), a) -> \((_, j, _, _, _), b) -> compare i j)) .
>             (sortBy (\((i, _, _, _, _), a) -> \((j, _, _, _, _), b) -> compare i j))

> -- Boundary builders

> class BuildBoundary d ix dyn g where
>    buildBoundary :: Dimensionality d -> BoundaryFun ix a dyn -> (Index d, Index d) -> g -> [(Index d, a)]

     buildBoundary :: Dimensionality d -> ((t, g) -> a) -> (Index d, Index d) -> g -> [(Index d, a)]

> instance (ReifiableIx (Nat n) Int) => BuildBoundary (Dim d) (Nat n) (Dynamic g) g where
>     buildBoundary d (Dynamic f) (x0, xn) grid =
>         let
>             x = typeToIntIx (undefined::(Nat n))
>             x' = if (x>0) then (x+xn)
>                           else (x0+x)
>         in
>             [(x' , f (typeToSymIx (undefined::(Nat n)), grid))]

> instance (ReifiableIx (Nat n) Int) => BuildBoundary (Dim d) (Nat n) Static g where
>     buildBoundary d (Static f) (x0, xn) grid =
>         let
>             x = typeToIntIx (undefined::(Nat n))
>             x' = if (x>0) then (x+xn)
>                           else (x0+x)
>         in
>             [(x' , f (typeToSymIx (undefined::(Nat n))))]
     
> instance (ReifiableIx (Nat n) Int, ReifiableIx (Nat m) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (Nat n, Nat m) (Dynamic g) g  where
>     buildBoundary d (Dynamic f) ((x0, y0), (xn, yn)) grid = 
>         let 
>             x = typeToIntIx (undefined::(Nat n))
>             y = typeToIntIx (undefined::(Nat m))
>             x' = if (x>0) then (x+xn) 
>                           else (x0+x)
>             y' = if (y>0) then (y+yn)
>                           else (y0+y)
>          in
>             [((x, y), f ((typeToSymIx (undefined::(Nat n)), typeToSymIx (undefined::(Nat m))), grid))] 


> instance (ReifiableIx (Nat n) Int, ReifiableIx (Nat m) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (Nat n, Nat m) Static g where
>     buildBoundary d (Static f) ((x0, y0), (xn, yn)) grid = 
>         let 
>             x = typeToIntIx (undefined::(Nat n))
>             y = typeToIntIx (undefined::(Nat m))
>             x' = if (x>0) then (x+xn) 
>                           else (x0+x)
>             y' = if (y>0) then (y+yn)
>                           else (y0+y)
>          in
>             [((x, y), f (typeToSymIx (undefined::(Nat n)), typeToSymIx (undefined::(Nat m))))]

> instance (ReifiableIx (Nat n) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (Nat n, Int) (Dynamic g) g where
>     buildBoundary d (Dynamic f) ((x0, y0), (xn, yn)) grid =
>         let
>             x = typeToIntIx (undefined::(Nat n))
>             x' = if (x>0) then (x+xn)
>                           else (x0+x)
>         in
>             map (\y -> ((x', y),
>                 f ((typeToSymIx (undefined::(Nat n)), y), grid))) (range (y0, yn))

> instance (ReifiableIx (Nat n) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (Nat n, Int) Static g where
>     buildBoundary d (Static f) ((x0, y0), (xn, yn)) grid =
>         let
>             x = typeToIntIx (undefined::(Nat n))
>             x' = if (x>0) then (x+xn)
>                           else (x0+x)
>         in
>             map (\y -> ((x', y),
>                 f (typeToSymIx (undefined::(Nat n)), y))) (range (y0, yn))

> instance (ReifiableIx (Nat m) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (Int, Nat m) (Dynamic g) g where
>     buildBoundary d (Dynamic f) ((x0, y0), (xn, yn)) grid =
>         let
>             y = typeToIntIx (undefined::(Nat m))
>             y' = if (y>0) then (y+yn)
>                           else (y0+y)
>         in
>             map (\x -> ((x, y'),
>                  f ((x, typeToSymIx (undefined::(Nat m))), grid))) (range (x0, xn))

> instance (ReifiableIx (Nat m) Int) =>
>          BuildBoundary ((Dim d) :* (Dim d')) (Int, Nat m) Static g where
>     buildBoundary d (Static f) ((x0, y0), (xn, yn)) grid =
>         let
>             y = typeToIntIx (undefined::(Nat m))
>             y' = if (y>0) then (y+yn)
>                           else (y0+y)
>         in
>             map (\x -> ((x, y'),
>                  f (x, typeToSymIx (undefined::(Nat m))))) (range (x0, xn))
