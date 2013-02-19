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

> type family Dynamism t t'
> type instance Dynamism Static Static = Static
> type instance Dynamism Static Dynamic = Dynamic
> type instance Dynamism Dynamic Static = Dynamic
> type instance Dynamism Dynamic Dynamic = Dynamic

Maps "absolute" index types to "relative" index types, i.e. Int -> Pos (Zn)

> type family AbsToReln t
> type instance AbsToReln Int = IntT (Pos Zn)
> type instance AbsToReln (IntT n) = IntT n
> type instance AbsToReln (a, b) = (AbsToReln a, AbsToReln b)
> type instance AbsToReln (a, b, c) = (AbsToReln a, AbsToReln b, AbsToReln c)

 type family Append l l'
 type instance Append Nil ys = ys
 type instance Append (Cons x xs) ys = Cons x (Append xs ys)

 appendBP :: BoundaryListP b d a -> BoundaryListP b' d a -> BoundaryListP (Append b b') d a
 appendBP NilBP y = y
 appendBP (ConsBP x xs) ys = ConsBP x (appendBP xs ys)


Type functions for combining inductively defined boundary info

> type family Origin d
> type instance Origin (Dim d) = IntT (Pos Zn)
> type instance Origin ((Dim d) :* (Dim d')) = (IntT (Pos Zn), IntT (Pos Zn))
> type instance Origin ((Dim d) :* ((Dim d') :* (Dim d''))) = (IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn))
> type instance Origin ((Dim x) :* ((Dim y) :* ((Dim z) :* (Dim w)))) = (IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn))
> type instance Origin ((Dim x) :* ((Dim y) :* ((Dim z) :* ((Dim w) :* (Dim u))))) = (IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn))
     
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


