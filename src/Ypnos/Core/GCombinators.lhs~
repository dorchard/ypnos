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

> module Ypnos.Core.GCombinators where

> import Ypnos.Core.Combinators

> import Ypnos.Core.Grid

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import Data.List

> import GHC.Prim

> import Debug.Trace


> class YpnosContainer (g :: * -> * -> *) x y where
>     type Inv g x y a :: Constraint 
>     type InvF g x y a b :: Constraint

>     type Plus g x y 
>     type Unit g 

>     current :: Inv g x y a => g (Unit g) a -> a
>     promote :: (Inv g x y a, Inv g x y b, InvF g x y a b) => (g x a -> b) -> g (Plus g x y) a -> g y b

> data Grid' d i a where
>     Grid' :: Grid d b dyn a -> Grid' d (b, dyn) a

> unGrid :: Grid' d (b, dyn) a -> Grid d b dyn a
> unGrid (Grid' g) = g

(Nil, Static) 

 type instance Plus (Grid' d) (b, dyn) (Nil, Static) = (b, dyn)

> instance Dimension d => YpnosContainer (Grid' d) (b, dyn) (Nil, Static) where
>     type Inv (Grid' d) (b, dyn) (Nil, Static) a = IArray UArray a
>     type InvF (Grid' d) (b, dyn) (Nil, Static) a b = ()

>     type Unit (Grid' d) = (Nil, Static)
>     type Plus (Grid' d) (b, dyn) (Nil, Static) = (b, dyn)

>     current (Grid' g) = indexC g
>     promote k (Grid' g) = Grid' $ run (k . Grid') g

> instance (RunGridA dyn, Dimension d) => YpnosContainer (Grid' d) (Cons b bs, dyn) (Cons b bs, dyn) where
>     type Inv (Grid' d) (Cons b bs, dyn) (Cons b bs, dyn) x = IArray UArray x
>     type InvF (Grid' d) (Cons b bs, dyn) (Cons b bs, dyn) x y = (x ~ y)

>     type Plus (Grid' d) (Cons b bs, dyn) (Cons b bs, dyn) = (Cons b bs, dyn)

>     current (Grid' g) = indexC g
>     promote k (Grid' g) = Grid' $ runA (k . Grid') g

