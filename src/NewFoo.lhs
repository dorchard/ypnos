> {-# LANGUAGE GADTs, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr
> import Data.HList
> import Unsafe.Coerce

> import System.IO

> import Ypnos.Core.Boundary
> import Ypnos.Core.Grid
> import Ypnos.Core.Types
> import Ypnos.Core.Dimensions

> data GridN d b cfs a where
>    GridN :: (UArray (Index d) a) ->    -- Array of values
>            Dimensionality d ->        -- Dimensionality term
>            Index d ->                 -- Cursor ("current index") 
>            (Index d, Index d) ->      -- Lower and upper bounds of extent
>            BoundaryList ixs d a ->    -- Boundary information
>            [Index d]            ->    -- Cache of coeffect (set/list of indices)
>            GridN d ixs cfs a 
>    Values :: [(Index d, a)] ->
>              GridN d ixs cfs a

 {-# INLINE index1D #-}
 index1D :: (Safe (IntT n) (Absify b), IArray UArray a) => IntT n -> Int -> Grid (Dim d) b a -> a
 index1D _ n (Grid arr d x _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n))

 index1D :: IntT n -> Int -> GridN (Dim d) b ixs a -> GridN (Dim d) b (HCons n ixs) a
 index1D _ i (GridN a d c bs bxs is) = GridN a d c bs bxs (i:is)

> index1D :: IntT n -> Int -> GridN (Dim d) b ixs a -> GridN (Dim d) b (HCons n ixs) a
> index1D _ _ (GridN arr d x b bix ixs) = GridN arr d x b bix ixs

> class FillValues d ixs where
>     fillValues :: IArray UArray a => (GridN d b ixs a -> b) -> GridN d b ixs' a -> GridN d b ixs a

> instance FillValues d HNil where
>     fillValues f _ = Values []

> instance (ReifiableIx n Int, FillValues (Dim d) xs) => FillValues (Dim d) (HCons (IntT n) xs) where
>     fillValues f g@(GridN arr d x _ _ _) = let Values bs = fillValues ((unsafeCoerce f) :: (GridN (Dim d) b xs a -> b)) g 
>                                                ix = typeToIntIx (undefined::n)
>                                                b = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + ix))
>                                            in Values $ (ix, b):bs


 index1D _ n (Grid arr d x _ _ cache) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n)

> run :: (IArray UArray y, Dimension d) => (GridN d b ixs x -> y) -> Grid d b HNil x -> Grid d Nil HNil y
> run f g@(GridN arr d c (b1, b2) boundaries _) = let gv' = fillValues f g
>                                                in undefined

 run f (Grid arr d c (b1, b2) boundaries) =
            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
                arr' = array (b1, b2) dats'
            in Grid arr' d c (b1, b2) NilB


