> module Ypnos (grid, listGrid, gridNoBoundary, listGridNoBoundary, gridData, 
>               indexC, index1D, index2D, index3D, (!!!), unsafeIndex2D, unsafeIndex1D, size,
>               fun, boundary,
>               run, runA,
>               apply, applyConvert
>               ) where

> import Prelude hiding (iterate)

 import Ypnos.Expr.Dimension
 import Ypnos.Expr.Boundary

> import Ypnos.Expr.Expr
> import Ypnos.Expr.Fun
> import Ypnos.Expr.Boundary

> import Ypnos.Core.Grid
> import Ypnos.Core.Combinators
> import Ypnos.Core.GCombinators



