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

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import Data.List

Computes the dynamism of a boundary list

> type family DynamismP t t'
> type instance DynamismP Static Static = Static
> type instance DynamismP Static Dynamic = Dynamic
> type instance DynamismP Dynamic Static = Dynamic
> type instance DynamismP Dynamic Dynamic = Dynamic

> type family Dynamism t
> type instance Dynamism Nil = Static
> type instance Dynamism (Cons (ix, dyn) ixs) = DynamismP dyn (Dynamism ixs)

Maps "absolute" index types to "relative" index types, i.e. Int -> Pos (Zn)

> type family AbsToReln t
> type instance AbsToReln Int = IntT (Pos Zn)
> type instance AbsToReln (IntT n) = IntT n
> type instance AbsToReln (a, b) = (AbsToReln a, AbsToReln b)
> type instance AbsToReln (a, b, c) = (AbsToReln a, AbsToReln b, AbsToReln c)

> type family Absify t
> type instance Absify Nil = Nil
> type instance Absify (Cons ix ixs) = Cons (AbsToReln ix) (Absify ixs)

 type family Append l l'
 type instance Append Nil ys = ys
 type instance Append (Cons x xs) ys = Cons x (Append xs ys)

 appendBP :: BoundaryListP b d a -> BoundaryListP b' d a -> BoundaryListP (Append b b') d a
 appendBP NilB y = y
 appendBP (ConsBP x xs) ys = ConsBP x (appendBP xs ys)



