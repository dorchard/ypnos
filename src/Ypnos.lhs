> module Ypnos (grid, listGrid, gridNoBoundary, listGridNoBoundary, 


>               -- General Ypnos combinators
>               runA, run, 
>               reduce, runReduceSimple, mkReducer, 
>               index,
>               getData, size,
>               zipC,
>               unzipC,

>               -- Grid specific
>               indexC, index1D, index2D, index3D, (!!!), 


>               -- Expression consutrctors
>               funCPU, boundary,
>               -- CPUArr(..), GPUArr(..),
>               -- RunGrid,
>               --RunCon, DataConst, GridList, ListConst,

>               Nat(..), IntT(..), Zn, S, Neg, Pos,
>               X(..),Y(..), Dimensionality(..), Dim, (:*),
>               DimIdentifier,
>               BoundaryFun(..), Static, Dynamic, 
>               BoundaryList(..), 
>               Size(..)
>               ) where

> import Prelude hiding (iterate)

 import Ypnos.Expr.Dimension
 import Ypnos.Expr.Boundary

> import Ypnos.Expr.Expr
> import Ypnos.Expr.Fun
> import Ypnos.Expr.Boundary

> import Ypnos.Core.Grid
> import Ypnos.Core.Combinators
> import Ypnos.Core.Dimensions
> import Ypnos.Core.Types
> import Ypnos.Core.Boundary

