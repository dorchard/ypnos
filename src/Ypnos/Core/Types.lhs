> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UndecidableInstances #-}


> module Ypnos.Core.Types where

Predicate for enforcing safe indexing

> class Safe i b

1D Safety

> instance Safe (IntT (Pos Zn)) b

> instance (Safe (IntT (Pred n)) b,
>           InBoundary (IntT n) b) => Safe (IntT n) b

2D Safety

> instance Safe (IntT (Pos Zn), IntT (Pos Zn)) b

> instance (Safe (IntT (Pred n), IntT n') b,
>           Safe (IntT n, IntT (Pred n')) b,
>           InBoundary (IntT n, IntT n') b) => Safe (IntT n, IntT n') b

 3D Safety

> instance Safe (IntT (Pos Zn), IntT (Pos Zn), IntT (Pos Zn)) b

> instance (Safe (IntT (Pred n), IntT n', IntT n'') b,
>           Safe (IntT n, IntT (Pred n'), IntT n'') b,
>           Safe (IntT n, IntT n', IntT (Pred n'')) b,
>           InBoundary (IntT n, IntT n', IntT n'') b) => Safe (IntT n, IntT n', IntT n'') b

> class InBoundary i ixs
> instance InBoundary i (Cons (i, dyn) ixs)                      -- Head matches
> instance InBoundary i ixs => InBoundary i (Cons (i', dyn) ixs) -- Head does not match, thus recurse


Type-level annotations for "static" and "dynamic" boundaries

> data Static 
> data Dynamic 

Type-level list (hetro-geneous list)

> data Nil
> data Cons a b

> {- 
> data List l where
>     Nil :: List Nil
>     Cons :: n -> List m -> List (Cons n m)
> -}


Type-level integers

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

Pretty print the numbers for debugging

> instance Show (Nat Zn) where
>     show Zn = "Z"

> instance Show (Nat n) => Show (Nat (S n)) where
>     show (S n) = "S" ++ (show n)

> instance (Show (Nat (S n)), Show (Nat n)) => Show (IntT (Neg (S n))) where
>     show (Neg n) = "-" ++ (show n)

> instance Show (Nat n) => Show (IntT (Pos n)) where
>     show (Pos n) = "+" ++ (show n)

Type-level functions on numbers 

> type family Pred n
> type instance Pred Int = Int
> type instance Pred (Neg (S (S n))) = Neg (S n)    -- Pred -(n+1) = -n
> type instance Pred (Neg (S Zn)) = Pos Zn          -- Pred -1 = 0
> type instance Pred (Pos Zn) = Pos Zn              -- Pred 0 = 0
> type instance Pred (Pos (S n)) = Pos n            -- Pred (n+1) = n

 Reify nat types as nat data and int data

> class ReifiableIx t t' | t -> t' where
>     typeToIntIx :: t -> t'
>     typeToSymIx :: t -> t

 instance ReifiableIx (Nat Int) Int where
     typeToIntIx = undefined

> instance ReifiableIx Int Int where
>     typeToIntIx _ = 0
>     typeToSymIx _ = 0

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

> instance (ReifiableIx a Int, ReifiableIx b Int) => 
>          ReifiableIx (a, b) (Int, Int) where
>     typeToIntIx _ = (typeToIntIx (undefined::(a)), typeToIntIx (undefined::(b)))
>     typeToSymIx _ = (typeToSymIx (undefined::(a)), typeToSymIx (undefined::(b)))

> instance (ReifiableIx (IntT a) Int, ReifiableIx (IntT b) Int, ReifiableIx (IntT c) Int) =>
>          ReifiableIx (IntT a, IntT b, IntT c) (Int, Int, Int) where
>     typeToIntIx _ = (typeToIntIx (undefined::(IntT a)), typeToIntIx (undefined::(IntT b)), typeToIntIx (undefined::(IntT c)))
>     typeToSymIx _ = (typeToSymIx (undefined::(IntT a)), typeToSymIx (undefined::(IntT b)),
>                                typeToSymIx (undefined::(IntT c)))

Maps "absolute" index types to "relative" index types, i.e. Int -> Pos (Zn)

> type family AbsToReln t
> type instance AbsToReln Int = IntT (Pos Zn)
> type instance AbsToReln (IntT n) = IntT n
> type instance AbsToReln (a, b) = (AbsToReln a, AbsToReln b)
> type instance AbsToReln (a, b, c) = (AbsToReln a, AbsToReln b, AbsToReln c)

> type family Absify t
> type instance Absify Nil = Nil
> type instance Absify (Cons ix ixs) = Cons (AbsToReln ix) (Absify ixs)