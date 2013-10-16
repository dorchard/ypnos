> {-# LANGUAGE GADTs, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeOperators #-}

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr
> import Data.HList
> import Unsafe.Coerce

> import System.Environment
> import PPM.Image


> import System.IO

> import Ypnos
> import Ypnos.Core.Grid
> import Ypnos.Core.Boundary
> import Ypnos.Core.Grid
> import Ypnos.Core.Types
> import Ypnos.Core.Dimensions

> data GridN d b cfs a where

    May be unecesseat

    GridN :: (UArray (Index d) a) ->    -- Array of values
            Dimensionality d ->        -- Dimensionality term
            Index d ->                 -- Cursor ("current index") 
            (Index d, Index d) ->      -- Lower and upper bounds of extent
            BoundaryList ixs d a ->    -- Boundary information
            [Index d]            ->    -- Cache of coeffect (set/list of indices)
            GridN d ixs cfs a 

>    Values :: [(Index d, a)] ->   -- Index d might be elided here
>              GridN d ixs cfs a

 {-# INLINE index1D #-}
 index1D :: (Safe (IntT n) (Absify b), IArray UArray a) => IntT n -> Int -> Grid (Dim d) b a -> a
 index1D _ n (Grid arr d x _ _) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n))

 index1D :: IntT n -> Int -> GridN (Dim d) b ixs a -> GridN (Dim d) b (HCons n ixs) a
 index1D _ i (GridN a d c bs bxs is) = GridN a d c bs bxs (i:is)

 foo g = let g' = index1D (Pos (S Zn)) 1 g 
             g'' = index1D (Pos $ S (S Zn)) 2 g'
         in g''

 index1D :: IntT n -> Int -> GridN (Dim d) b ixs a -> GridN (Dim d) b (HCons n ixs) a
 index1D _ _ (GridN arr d x b bix ixs) = GridN arr d x b bix ixs

 index1D _ n (Grid arr d x _ _ cache) = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + n)


This is probably more what we want: 

> fooB g@(Values [(lix, l), (rix, r)]) 
>          = let g' = ix1D (Pos (S Zn)) g 
>                g'' = ix1D (Pos $ S (S Zn)) g'
>            in l + r

> {-# INLINE ix1D #-}
> ix1D :: IntT n -> GridN (Dim d) b (HCons n ixs) a -> GridN (Dim d) b ixs a
> ix1D _ (Values x) = Values x

> {-# INLINE ix2D #-}
> ix2D :: (IntT n, IntT m) -> GridN (Dim d :* Dim d') b (HCons (n, m) ixs) a -> GridN (Dim d :* Dim d') b ixs a
> ix2D _ (Values x) = Values x

> endIxing :: GridN d b HNil a 
> endIxing = Values []

 index1DB _ _ (GridN arr d x b bix ixs) = GridN arr d x b bix ixs

> class FillValues d ixs where

     fillValues :: IArray UArray x => (GridN d b ixs x -> y) -> GridN d b ixs' x -> GridN d b ixs x

>     
>     fillValues :: IArray UArray x => (GridN d b ixs x -> y) -> Grid d b x -> GridN d b ixs x

> instance FillValues d HNil where

>     {-# INLINE fillValues #-}
>     fillValues f _ = Values []

     fillValues2 f _ = Values []

> instance (ReifiableIx (IntT n) Int, ReifiableIx (IntT m) Int, FillValues (Dim d :* Dim d') xs) => FillValues (Dim d :* Dim d') (HCons (n, m) xs) where

>     {-# INLINE fillValues #-}
>     fillValues f g@(Grid arr d (x, y) _ _) = 
>                                         let Values bs = fillValues ((unsafeCoerce f) :: (GridN (Dim d :* Dim d') b xs a -> b)) g 
>                                             ixX = typeToIntIx (undefined::(IntT n))
>                                             ixY = typeToIntIx (undefined::(IntT m))
>                                             b = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + ixX, y + ixY))
>                                         in Values $ ((ixX, ixY), b):bs

> instance (ReifiableIx (IntT n) Int, FillValues (Dim d) xs) => FillValues (Dim d) (HCons n xs) where

>     {-# INLINE fillValues #-}
>     fillValues f g@(Grid arr d x _ _) = let Values bs = fillValues ((unsafeCoerce f) :: (GridN (Dim d) b xs a -> b)) g 
>                                             ix = typeToIntIx (undefined::(IntT n))
>                                             b = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) (x + ix))
>                                         in Values $ (ix, b):bs




> run2 :: (IArray UArray x, IArray UArray y, Ix (Index d), FillValues d ixs) => (GridN d b ixs x -> y) -> Grid d b x -> Grid d Nil y
> run2 f g@(Grid arr d c (b1, b2) boundaries) = let --gv' = fillValues2 f g
>                                                  dats' = map (\c' -> (c', let gv' = fillValues f (Grid arr d c' (b1, b2) boundaries)
>                                                                          in f gv')) (range (b1, b2))
>                                                  arr' = array (b1, b2) dats'
>                                               in Grid arr' d c (b1, b2) NilB

> {-# INLINE windowConcatMap2 #-}
> windowConcatMap2 :: ([a] -> [b]) -> [a] -> [b]
> windowConcatMap2 f [] = []
> windowConcatMap2 f [x] = f [x]
> windowConcatMap2 f (x:x':xs) = (f [x, x']) ++ (windowConcatMap2 f xs)

> runUnroll :: (IArray UArray x, IArray UArray y, Ix (Index d), FillValues d ixs) => (GridN d b ixs x -> y) -> Grid d b x -> Grid d Nil y
> runUnroll f g@(Grid arr d c (b1, b2) boundaries) = let --gv' = fillValues2 f g
>                                                  dats' = windowConcatMap2 applyFun (range (b1, b2)) 
>                                                  applyFun [c'] = [(c', let gv' = fillValues f (Grid arr d c' (b1, b2) boundaries)
>                                                                        in f gv')]
>                                                  applyFun [c', c''] = let gv' = fillValues f (Grid arr d c' (b1, b2) boundaries)
>                                                                           gv'' = fillValues f (Grid arr d c'' (b1, b2) boundaries)
>                                                                       in [(c', f gv'), (c'', f gv'')]
>                                                  arr' = array (b1, b2) dats'
>                                               in Grid arr' d c (b1, b2) NilB

> runOld f (Grid arr d c (b1, b2) boundaries) =
>            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                arr' = array (b1, b2) dats'
>            in Grid arr' d c (b1, b2) NilB


> main = do -- Read image file
>        argv <- getArgs
>        (x, y, img) <- read_ppm (argv!!0)
>        -- Run Ypnos stencil
>        let g = listGrid (Dim X :* Dim Y) (0, 0) (x, y) img zeroBound
>        let g' = case (argv!!1) of
>                   "normal" -> run laplace2D g
>                   "new" -> run2 laplace2D2 g
>                   "unroll" -> runUnroll laplace2D2 g
>                   _ -> error "Choose normal, new, or unroll"
>        -- Write data to image file
>        write_ppm ((argv!!0)++"-ypnos"++argv!!1) x y (gridData g')
          

> laplace2D2 g@(Values [(_, l), (_, r), (_, t), (_, b), (_, c)])
>              = let g1 = ix2D (Neg (S Zn), Pos Zn) g
>                    g2 = ix2D (Pos (S Zn), Pos Zn) g1
>                    g3 = ix2D (Pos Zn, Pos (S Zn)) g2
>                    g4 = ix2D (Pos Zn, Neg (S Zn)) g3
>                    g5 = ix2D (Pos Zn, Pos Zn) g4
>                    _ = [endIxing, g5]  -- forces typ equality between a HNil gridN and g5! 
>                in t + l + r + b - 4.0*c 

> laplace2D = [fun| X*Y:| _  t  _ |
>                       | l @c  r |
>                       | _  b  _ | -> t + l + r + b - 4.0*c |]

> zeroBound = [boundary| Double (-1, *j) -> 0.0
>                               (+1, *j) -> 0.0 
>                               (*i, -1) -> 0.0 
>                               (*i, +1) -> 0.0 |]


First test with no unrolling
-- normal: 1.604, 1.593, 1.595
-- new:    1.718, 1.728, 1.763

-- inlined new: 1.527, 1.526, 1.536
-- inlined new unrolled: 1.611, 1.599, 1.599