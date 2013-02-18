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

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import Data.List

> import Debug.Trace

> -- Type level integers

> data Zn
> data S n 

> data Nat n where
>     Zn :: Nat Zn
>     S :: Nat n -> Nat (S n)

> data Neg n
> data Pos n

> data IntT n where
>     Neg :: Nat (S n) -> IntT (Neg (S n))
>     Pos :: Nat n -> IntT (Pos n)

 {-# INLINE natToInt #-}

> natToInt :: Nat n -> Int
> natToInt Zn = 0
> natToInt (S n) = 1 + natToInt n

{-# INLINE intTtoInt #-}

> intTtoInt :: IntT n -> Int
> intTtoInt (Pos n) = natToInt n
> intTtoInt (Neg n) = - natToInt n

> -- Some helpers, synonyms for numbers

> instance Show (Nat Zn) where
>     show Zn = "Z"

> instance Show (Nat n) => Show (Nat (S n)) where
>     show (S n) = "S" ++ (show n)

> instance (Show (Nat (S n)), Show (Nat n)) => Show (IntT (Neg (S n))) where
>     show (Neg n) = "-" ++ (show n)

> instance Show (Nat n) => Show (IntT (Pos n)) where
>     show (Pos n) = "+" ++ (show n)

> -- Type-level list (hetro-geneous list)

> data Nil
> data Cons a b

> {- 
> data List l where
>     Nil :: List Nil
>     Cons :: n -> List m -> List (Cons n m)
> -}

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

instance (Dimension (Dim d), Dimension (Dim d'), Dimension (Dim d'')) => (Dimension (Dim d :* (Dim d' :* Dim d'')))


> data Dim d 
> data (:*) d d'

> data Dimensionality d where
>     Dim :: d -> Dimensionality (Dim d)
>     (:*) :: Dimensionality (Dim d) -> Dimensionality d' -> Dimensionality (Dim d :* d')

> -- Indices terms

> type family Index t
> type instance Index () = ()
> type instance Index (Dim d) = Int
> type instance Index ((Dim d) :* (Dim d')) = (Int, Int)
> type instance Index ((Dim d) :* ((Dim d') :* (Dim d''))) = (Int, Int, Int)
> type instance Index ((Dim x) :* ((Dim y) :* ((Dim z) :* (Dim w)))) = (Int, Int, Int, Int)
> type instance Index ((Dim x) :* ((Dim y) :* ((Dim z) :* ((Dim w) :* (Dim u))))) = (Int, Int, Int, Int, Int)

> -- Grid data type

> data Grid d b dyn a where
>    Grid :: (UArray (Index d) a) ->                      -- Array of values
>            Dimensionality d ->                          -- Dimensionality term
>            Index d ->                                   -- Cursor ("current index") 
>            (Index d, Index d) ->                        -- Lower and upper bounds of extent
>            BoundaryList ixs dyn lower upper d a ->      -- Boundary information
>            Grid d ixs dyn a

> instance (Ix (Index d), IArray UArray a, 
>           Show (Index d), Show a) => Show (Grid d b dyn a) where
>     show (Grid arr d c (b1, b2) _) = (show arr)++"@"++(show c)++" ["++(show b1)++", "++(show b2)++"]"

> type family AbsToReln t
> type instance AbsToReln Int = IntT (Pos Zn)
> type instance AbsToReln (IntT n) = IntT n
> type instance AbsToReln (a, b) = (AbsToReln a, AbsToReln b)
> type instance AbsToReln (a, b, c) = (AbsToReln a, AbsToReln b, AbsToReln c)

> -- Boundaries functions and list tpes

> data Static 
> data Dynamic 

> data BoundaryFun d ix a dyn where
>     Static :: (ix -> a) -> BoundaryFun d ix a Static
>     Dynamic :: ((ix, (Grid d Nil Static a)) -> a) -> BoundaryFun d ix a Dynamic

> data BoundaryList b dyn lower upper d a where
>     NilB :: BoundaryList Nil Static (Origin d) (Origin d) d a
>     ConsB :: BuildBoundary d ix dyn => 
>              BoundaryFun d ix a dyn
>               -> BoundaryList b dyn' lower upper d a 
>               -> BoundaryList (Cons (AbsToReln ix) b) (Dynamism dyn dyn') (Lower (AbsToReln ix) lower) (Upper (AbsToReln ix) upper) d a

> -- Type functions for combining inductively defined boundary info

> type family Origin d
> type instance Origin (Dim d) = IntT (Pos Zn)
> type instance Origin ((Dim d) :* (Dim d')) = (IntT (Pos Zn), IntT (Pos Zn))
> type instance Origin ((Dim d) :* ((Dim d') :* (Dim d''))) = (IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn))
> type instance Origin ((Dim x) :* ((Dim y) :* ((Dim z) :* (Dim w)))) = (IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn))
> type instance Origin ((Dim x) :* ((Dim y) :* ((Dim z) :* ((Dim w) :* (Dim u))))) = (IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn))

> type family Dynamism t t'
> type instance Dynamism Static Static = Static
> type instance Dynamism Static Dynamic = Dynamic
> type instance Dynamism Dynamic Static = Dynamic
> type instance Dynamism Dynamic Dynamic = Dynamic
     
> type family Max t t'

> type instance Max Zn Zn = Zn
> type instance Max Zn (S n) = S n
> type instance Max (S n) Zn = S n
> type instance Max (S n) (S m) = S (Max n m)

> type instance Max (Pos n) (Pos m) = Pos (Max n m)
> type instance Max (Neg n) (Pos m) = Pos m
> type instance Max (Pos n) (Neg m) = Pos n
> type instance Max (Neg n) (Neg m) = Neg (Min n m)

 type instance Max Int (IntT (Pos n)) = IntT (Pos n)
 type instance Max Int (IntT (Neg n)) = Int
 type instance Max (IntT (Pos n)) Int = IntT (Pos n)
 type instance Max (IntT (Neg n)) Int = Int
 type instance Max Int Int = Int

> type family Upper a b
> type instance Upper (IntT a) (IntT b) = IntT (Max a b)
> type instance Upper (a, b) (c, d) = (Upper a c, Upper b d)
> type instance Upper (a, b, c) (d, e, f) = (Upper a d, Upper b e, Upper c f)
> type instance Upper (a, b, c, d) (e, f, g, h) = (Upper a e, Upper b f, Upper c g, Upper d h)

> type family Min t t'

> type instance Min Zn Zn = Zn
> type instance Min Zn (S n) = Zn
> type instance Min (S n) Zn = Zn
> type instance Min (S n) (S m) = S (Min n m)

> type instance Min (Pos n) (Pos m) = Pos (Min n m)
> type instance Min (Pos n) (Neg m) = Neg m
> type instance Min (Neg n) (Pos m) = Neg n
> type instance Min (Neg n) (Neg m) = Neg (Max n m)

 type instance Min Int (IntT (Pos n)) = Int
 type instance Min Int (IntT (Neg n)) = IntT (Neg n)
 type instance Min (IntT (Pos n)) Int = Int
 type instance Min (IntT (Neg n)) Int = IntT (Neg n)
 type instance Min Int Int = Int

> type family Lower a b
> type instance Lower (IntT a) (IntT b) = IntT (Min a b)
> type instance Lower (a, b) (c, d) = (Lower a c, Lower b d)
> type instance Lower (a, b, c) (d, e, f) = (Lower a d, Lower b e, Lower c f)
> type instance Lower (a, b, c, d) (e, f, g, h) = (Lower a e, Lower b f, Lower c g, Lower d h)

> -- Enforces safe indexing

> type family Pred n
> type instance Pred (Neg (S (S n))) = Neg (S n)    -- Pred -(n+1) = -n
> type instance Pred (Neg (S Zn)) = Pos Zn          -- Pred -1 = 0
> type instance Pred (Pos Zn) = Pos Zn              -- Pred 0 = 0
> type instance Pred (Pos (S n)) = Pos n            -- Pred (n+1) = n

> class Safe i b
> 
> -- 1D Safety

> instance Safe (IntT (Pos Zn)) b 

> instance (Safe (IntT (Pred n)) b,
>           InBoundary (IntT n) b) => Safe (IntT n) b 

> -- 2D Safety

> instance Safe (IntT (Pos Zn), IntT (Pos Zn)) b 

> instance (Safe (IntT (Pred n), IntT n') b,
>           Safe (IntT n, IntT (Pred n')) b,
>           InBoundary (IntT n, IntT n') b) => Safe (IntT n, IntT n') b

> -- 3D Safety

> instance Safe (IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn)) b

> instance (Safe (IntT (Pred n), IntT n', IntT n'') b,
>           Safe (IntT n, IntT (Pred n'), IntT n'') b,
>           Safe (IntT n, IntT n', IntT (Pred n'')) b,
>           InBoundary (IntT n, IntT n', IntT n'') b) => Safe (IntT n, IntT n', IntT n'') b  

> class InBoundary i ixs
> instance InBoundary i (Cons i ixs)                    -- Head matches
> instance InBoundary i ixs => InBoundary i (Cons i' ixs)   -- Head does not match, thus recurse

> -- Computes the values of a boundary region, given a boundary list

> boundMap :: (IndexOps (Index d)) => Dimensionality d ->
>             BoundaryList ixs dyn lower upper d a -> Grid d Nil Static a ->
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

> instance ReifiableIx (Nat (S n)) Int => ReifiableIx (IntT (Neg (S n))) Int where 
>     typeToIntIx _ = - (typeToIntIx (undefined::(Nat (S n))))
>     typeToSymIx _ = Neg (typeToSymIx (undefined::(Nat (S n))))

> instance ReifiableIx (Nat n) Int => ReifiableIx (IntT (Pos n)) Int where 
>     typeToIntIx _ = typeToIntIx (undefined::(Nat n))
>     typeToSymIx _ = Pos (typeToSymIx (undefined::(Nat n)))

> instance (ReifiableIx (IntT a) Int, ReifiableIx (IntT b) Int) => 
>          ReifiableIx (IntT a, IntT b) (Int, Int) where
>     typeToIntIx _ = (typeToIntIx (undefined::(IntT a)), typeToIntIx (undefined::(IntT b)))
>     typeToSymIx _ = (typeToSymIx (undefined::(IntT a)), typeToSymIx (undefined::(IntT b)))

> instance (ReifiableIx (IntT a) Int, ReifiableIx (IntT b) Int, ReifiableIx (IntT c) Int) =>
>          ReifiableIx (IntT a, IntT b, IntT c) (Int, Int, Int) where
>     typeToIntIx _ = (typeToIntIx (undefined::(IntT a)), typeToIntIx (undefined::(IntT b)), typeToIntIx (undefined::(IntT c)))
>     typeToSymIx _ = (typeToSymIx (undefined::(IntT a)), typeToSymIx (undefined::(IntT b)),
>                                typeToSymIx (undefined::(IntT c)))


> -- Various operations on indices

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

> -- Boundary builders

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