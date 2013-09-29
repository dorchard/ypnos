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

> module Ypnos.Core.Run where

> import Ypnos.Core.Grid
> import Ypnos.Core.Dimensions
> import Ypnos.Core.Boundary
> import Ypnos.Core.Types
> import Ypnos.Core.Dimensions

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import Data.Monoid
> import Data.List

> import Debug.Trace

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

 runUnroll :: (IArray UArray y, Dimension d) => 
              (Grid d b x -> Grid' x) -> Grid d b x -> Grid d Nil y
 runUnroll 

General gather over a grid. Note, this destroys the boundaries

> run :: (IArray UArray y, Dimension d) => (Grid d b x -> y) -> Grid d b x -> Grid d Nil y
> run f (Grid arr d c (b1, b2) boundaries) =
>            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                arr' = array (b1, b2) dats'
>            in Grid arr' d c (b1, b2) NilB


Gather over a grid, preserving boundaries

> class RunGridA dyn where
>     runA :: (dyn ~ Dynamism b, IArray UArray a, Dimension d) => 
>             (Grid d b a -> a) -> Grid d b a -> Grid d b a

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

