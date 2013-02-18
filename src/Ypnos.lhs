> module Ypnos (grid, listGrid, gridNoBoundary, listGridNoBoundary, gridData, size,
>               indexC, index1D, index2D, index3D, (!!!), unsafeIndex2D, unsafeIndex1D,
>               funCPU, safeFun, unsafeFun, boundary,
>               run, runG, reduceG, mkReducer,
>               ) where

> import Prelude hiding (iterate)

 import Ypnos.Expr.Dimension
 import Ypnos.Expr.Boundary

> import Ypnos.Expr.Expr
> import Ypnos.Expr.Fun
> import Ypnos.Expr.Boundary

> import Ypnos.Core.Grid
> import Ypnos.Core.Combinators

> import Ypnos.CUDA



