> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> {-# LANGUAGE FlexibleInstances #-}

> class Foo a where
>     type AssocType a
>     foo :: (AssocType a, AssocType a) -> AssocType a

This defines a class "Foo" with an "associated type" (as they are called). Associated
types are a form of "type family" which is associated to a class. In unassociated form they look like

type family AsscoType' a
type instance AssocType' Foo = ....

Here is an instance of Foo: 

> instance Foo Int where
>     type AssocType Int = [Bool]
>     foo (x, y) = map (\(x', y') -> x' && y') (zip x y)


(for more see: http://www.haskell.org/ghc/docs/7.6.1/html/users_guide/type-families.html)

The following encodes is what we would need to abstract the "stencil function" type:

> class YpnosGrid (a :: * -> *) where
>     type StencilFun a x y

Note that a "kind signature" is used here on "a". Kinds are the "types" of types - so this
tells us that 'a' takes a type to a type (it's a unary type constructor). These aren't always
needed (often the kind can be inferred from how a type variable is used, for example in the
signatures of class methods- however here I haven't included any methods yet so the kind is
unknown and we need to tell Haskell because this version of GHC doesn't support "kind polymorphism").

Ok, so StencilFun is a trinary function :(

So we could write the following which inserts quantifies the 'a' in its own scope.

> type (:~>) x y = forall a . YpnosGrid a => StencilFun a x y

And Haskell syntax allows us to use :~> infix as it is binary, e.g.:

> foo' :: x :~> y 
> foo' = undefined

(Note this is a nice hack for testing a type by using undefined, which has type:

  undefined :: a

i.e. any type).

A pure grid can then have the usual stencil function type for example:

> data Grid a = Grid a -- temporary definition

> instance YpnosGrid Grid where
>     type StencilFun Grid x y = Grid x -> y


An accelerate grid might be a bit different:

> class Stencil (t :: * -> *)  -- Class of valid stencil types 

> data AGrid a = AGrid a -- temporary definition representing Accelerate's array type
> data Exp a = Exp a -- temporary definition represeting Accelerate's expression tree type

Now we want Accelerate stencils to be something like the following:

> type AStencil x y = forall t . Stencil t => t x -> Exp y

Unfortunately, a "forall" type can't be used for a type family instance :( so we can't do the following:

 instance YpnosGrid AGrid where
     type StencilFun AGrid x y = SF x y -- accelerate stencil functions

Ok, so this is one problem. Another is when we actually try to define "run". We might try the following:

> class YpnosGrid a => YpnosRun a where
>     run :: (x :~> y) -> a x -> a y

Let's do the instance for 'Grid' above. Since 'Grid' is super simple (it's basically a box around a value, so
applying a function |Grid a -> b| is super easy. We might reasonably write:

 instance YpnosRun Grid where
     run f x = Grid (f x)

*But* this can't type check (try uncommenting it) because the type of x is potentially different to the type
that f expects since that is wrapped in its own forall. So, the type of 'run' here is:

run :: (forall a . YpnosGrid a => StencilFun a x y) -> Grid x -> Grid y

The 'a' is still quantified and nothing can tell it to unify with Grid here. So... let's ditch this
method. Another approach might be to make the "stencil function" type a parameter of the 'YpnosGrid' class
which will give some more flexibility

> class YpnosGridP g sfun where
>     runP :: (sfun x y) -> g x -> g y
 
(Note, note using the MultiParamTypeClasses extension)

We can't do the following because we can never use type synonyms in an unapplied form:

 instance YpnosGridP Grid (:~>) where

so we must wrap this in a data type:

> data (:~~>) x y = GridStencilFun (Grid x -> y)

Then we can do:

> instance YpnosGridP Grid (:~~>) where
>     runP (GridStencilFun f) x = Grid (f x)

And for the accelerate grids that can work for different "stencil types" we can do

> data AGSFun t x y = AGridStencilFun (t x -> Exp y)

> data Stencil3 x = Stencil3 (x, x, x)

> instance YpnosGridP AGrid (AGSFun Stencil3) where
>     runP (AGridStencilFun f) (AGrid x) =
>                let (Exp x') = (f (Stencil3 (x, x, x))) -- this is just for the sake of the example
>                in AGrid x'

(btw, I had to turn on FlexibleInstances as I'm using type constructors inside of type constructors in the
instance head)

This method is pretty good and might work out well: notice how we can provide different instances
depending on the stencil function type e.g.

> data Stencil5 x = Stencil5 (x, x, x, x, x)

> instance YpnosGridP AGrid (AGSFun Stencil5) where
>     runP (AGridStencilFun f) (AGrid x) =
>                let (Exp x') = (f (Stencil5 (x, x, x, x, x))) -- this is just for the sake of the example
>                in AGrid x'

So we can implement the behaviour of different stencils here depending
on the type (i.e. overloading, also called "ad-hoc polymorphism")
