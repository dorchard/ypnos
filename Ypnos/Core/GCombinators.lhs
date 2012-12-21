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

> module Ypnos.Core.GCombinators where

> import Ypnos.Core.Combinators

> import Ypnos.Core.Grid


import Data.Semigroup.Reducer
import Control.Functor.Pointed

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import Data.List

> import GHC.Prim

> import Debug.Trace

> class (Functor f) => Copointed f where
>    extract :: f a -> a

> class RComonad c where
>     type RCObjs c a :: Constraint
>     type RCMorphs c a b :: Constraint
>     rcounit :: (RCObjs c a) => c a -> a
>     rcobind :: (RCObjs c a, RCObjs c b, RCMorphs c a b) => (c a -> b) -> c a -> c b

> type Inv c a = (RCObjs c a, CObjs c a)
> type InvF c a b = (RCMorphs c a b, CMorphs c a b)

> type family CObjs (c :: * -> *) a :: Constraint
> type family CMorphs (c :: * -> *) a b :: Constraint

> class RComonad c => Container c where
>     current :: (Inv c a) => c a -> a
>     current = rcounit

>     apply :: (Inv c a, Inv c b, InvF c a b) => (c a -> b) -> c a -> c b
>     apply = rcobind
    

> class (Container c) => NavigatableContainer c where
>     neighbourhood :: Inv c a => c a -> [Maybe a]

> class (Copointed (f c), Container c) => NavigatableContainer2 c f where
>     neighbourhoodF :: Inv c a => c a -> f c a


> class (Container (c b1), Container (c b2)) => BorderedContainer (c :: * -> * -> *) b1 b2 where
>     type Result c b1 b2 
>     applyConvert :: (Inv (c b1) a, Inv (c b2) a, Inv (c (Result c b1 b2)) b,
>                      InvF (c b1) a b) => (c b1 a -> b) -> c b2 a -> c (Result c b1 b2) b


     

> -------------------------- GRID -------------------------

> instance (Dimension d, RComonad (Grid d (b, dyn))) => Container (Grid d (b, dyn)) where
> instance RComonad (Grid (Dim d :* Dim d') b) => Container (Grid (Dim d :* Dim d') b) where

> type instance CObjs (Grid d i) a = IArray UArray a
> type instance CMorphs (Grid d i) a b = ()

> instance (Dimension d) => RComonad (Grid d (b, Dynamic)) where
>     type RCObjs (Grid d (b, Dynamic)) x = IArray UArray x
>     type RCMorphs (Grid d (b, Dynamic)) x y = x ~ y
>     rcounit = current
>     rcobind f (Grid arr d c (b1, b2) boundaries) = 
>            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                arr' = accum (curry snd) arr dats'
>                g0 = Grid arr' d c (b1, b2) NilB
>                es = boundMap d boundaries g0 b1 b2
>                (b1', b2') = bounds arr
>                arr'' = array (b1', b2') (dats'++es)
>            in
>                Grid arr'' d c (b1, b2) boundaries 

> instance (Dimension d) => RComonad (Grid d (b, Static)) where
>     type RCObjs (Grid d (b, Static)) x = IArray UArray x
>     type RCMorphs (Grid d (b, Static)) x y = x ~ y
>     rcounit = current
>     rcobind f (Grid arr d c (b1, b2) boundaries) = 
>            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
>                arr' = accum (curry snd) arr dats'
>            in  Grid arr' d c (b1, b2) boundaries

> instance (Container (Grid (Dim d :* Dim d') b)) => NavigatableContainer (Grid (Dim d :* Dim d') b) where
>     neighbourhood g = [index2Dsafe (-1, 0) g, index2Dsafe (1, 0) g,
>                        index2Dsafe (0, -1) g, index2Dsafe (0, 1) g]

> instance (b ~ b', dyn ~ dyn', Dimension d, RComonad (Grid d (b, dyn))) =>
>          BorderedContainer (Grid d) (b, dyn) (b', dyn') where
>     type Result (Grid d) (b, dyn) (b', dyn') = (Nil, Static)
>     applyConvert = run

 class (Copointed f, Container c) => NavigatableContainer2 c f where
     neighbourhoodF :: Inv c a => c a -> f (c a)

> data F c a = F (a, [c a])

 
 instance Copointed (F c) where
     extract (F (x, _)) = x
     

 instance Functor c => Functor (F c) where
     fmap f (F (a, cs)) = F (f a, fmap f cs)

 instance NavigatableContainer2 (Grid (Dim d :* Dim d') (b, dyn)) F where

> ---------
     


 class (Container (g x), Container (g y)) => ContainerApply (g :: * -> * -> *) (x :: *) (y :: *) (a :: *) (b :: *) c where
     type ResultA g x y a b (c :: Constraint)
     applys :: (Inv (g x) a, Inv (g y) a, Inv (g y) b,
                InvF (g x) a b, Inv (g (ResultA g x y a b c)) b) => (g x a -> b) -> g y a -> g (ResultA g x y a b c) b


 instance (RComonad (Grid d (b', dyn')), 
           Dimension d, RunGridA dyn, b ~ b', dyn ~ dyn') => ContainerApply (Grid d) (b, dyn) (b', dyn') a a (a ~ a) where
     type ResultA (Grid d) (b, dyn) (b', dyn') a a (a ~ a) = (b, dyn)
     applys = runA

 instance (RComonad (Grid d (b', dyn')),
           Dimension d, b ~ b', dyn ~ dyn') => ContainerApply (Grid d) (b, dyn) (b', dyn') x y () where
     type ResultA (Grid d) (b, dyn) (b', dyn') x y () = (Nil, Static)
     applys = run


 class Container (g :: * -> * -> *) x y where
     type Inv g a :: Constraint 
     type InvF g a b :: Constraint

     type Result g x y
     type ResultA g x y

     current :: Inv g a => g e a -> a
     promote :: (Inv g a, Inv g b, InvF g a b) => (g x a -> b) -> g y a -> g (Result g x y) b
     promoteA :: (Inv g a, InvF g a a) => (g x a -> a) -> g y a -> g (ResultA g x y) a



 instance (b ~ b', Dimension d, dyn ~ Dynamic) => Container (Grid d) (b, dyn) (b', Dynamic) where
     type Inv (Grid d) a = IArray UArray a
     type InvF (Grid d) a b = IArray UArray b

     type Result (Grid d) (b, dyn) (b', Dynamic) = (Nil, Static) 
     type ResultA (Grid d) (b, dyn) (b', Dynamic) = (b', Dynamic)

     current = indexCt
     promote = run
     promoteA f (Grid arr d c (b1, b2) boundaries) = 
            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
                arr' = accum (curry snd) arr dats'
                g0 = Grid arr' d c (b1, b2) NilB
                es = boundMap d boundaries g0 b1 b2
                (b1', b2') = bounds arr
                arr'' = array (b1', b2') (dats'++es)
            in
                Grid arr'' d c (b1, b2) boundaries

 instance (b ~ b', Dimension d, dyn ~ Static) => Container (Grid d) (b, dyn) (b', Static) where
     type Inv (Grid d) a = IArray UArray a
     type InvF (Grid d) a b = IArray UArray b

     type Result (Grid d) (b, dyn) (b', Static) = (Nil, Static) 
     type ResultA (Grid d) (b, dyn) (b', Static) = (b', Static)

     current = indexCt
     promote = run
     promoteA f (Grid arr d c (b1, b2) boundaries) = 
            let dats' = map (\c' -> (c', f (Grid arr d c' (b1, b2) boundaries))) (range (b1, b2))
                arr' = accum (curry snd) arr dats'
            in  Grid arr' d c (b1, b2) boundaries



