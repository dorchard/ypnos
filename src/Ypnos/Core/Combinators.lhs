> {-# LANGUAGE GADTs, TypeFamilies, MultiParamTypeClasses, ConstraintKinds, NoMonomorphismRestriction, 
>              TypeOperators, FunctionalDependencies, FlexibleContexts #-}

> module Ypnos.Core.Combinators where

> import GHC.Prim
> import Data.Monoid

> import Ypnos.Core.Types


> type family ElemInv (g :: * -> * -> *) x :: Constraint

Run

> class Run (g :: * -> * -> *) (fun :: * -> * -> *) | fun -> g where
>     type RunInv g fun b x y :: Constraint
>     
>     runA :: RunInv g fun b x x => ((g b x) `fun` x) -> g b x -> g b x
>     run  :: RunInv g fun b x y => ((g b x) `fun` y) -> g b x -> g Nil y

Reduce

> data Reducer g a c where
>     Reducer :: (ConstFun2 g a b b,  ConstFun2 g b b b, ConstFun1 g a c)
>               => (Fun2 g a b b)
>               -> (Fun2 g b b b)
>               -> b
>               -> (Fun1 g b c)
>               -> Reducer g a c
> mkReducer = Reducer 


> class Reduce (g :: * -> * -> *) where

>     type ConstFun1 g a b :: Constraint
>     type ConstFun2 g a b c :: Constraint
>     type Fun1 g a b
>     type Fun2 g a b c
>     reduce :: Reducer g a c -> g boundary a -> c

Simple reductions

> class RunReduceSimple (g :: * -> * -> *) where
>     runReduceSimple :: Monoid r => (g b x -> (r, y)) -> g b x -> (r, g Nil y)


Relative (safe) indexing

> class Indexing (g :: * -> * -> *) ix where
>     type IndexG g ix
>     type IxInv g ix a :: Constraint
>     index :: (Safe ix (Absify b), IxInv g ix a, ElemInv g a) => (IndexG g ix) -> g b a -> a

Deconstructors

> class Data (g :: * -> * -> *) where
>     type DataInv g a :: Constraint
>     getData :: DataInv g a => g b a -> [a]


> class Size (g :: * -> * -> *) where
>     type SizeAbs g
>     size :: g b a -> SizeAbs g

Zipping and unzipping

> class Zip (g :: * -> * -> *) where
>     type BZipC g b b' b'' :: Constraint
>     type BUnzipC g b :: Constraint

>     zipC :: (ElemInv g x, ElemInv g y, ElemInv g (x, y), BZipC g b b' b'') => g b x -> g b' y -> g b'' (x, y)
>     unzipC :: (ElemInv g x, ElemInv g y, ElemInv g (x, y), BUnzipC g b) => g b (x, y) -> (g b x, g b y)




