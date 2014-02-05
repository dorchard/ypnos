> {-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}

> module Ypnos.Backend.CPU where

> import Ypnos.Core.Types
> import Ypnos.Core.Dimensions
> import Ypnos.Core.Combinators
> import Ypnos.Core.Boundary

> import Ypnos.Core.Grid

> import Data.Array.IArray
> import Data.Array.Unboxed

> data CPUstencil x y = CPUstencil (x -> y) -- pure 

> instance (Dimension d) => Run (Grid d) CPUstencil where
>     type RunInv (Grid d) CPUstencil b x y = (IArray UArray y, RunGridA (Dynamism b))

>     runA (CPUstencil f) = runGA f
>     run  (CPUstencil f) = runG  f

