module Ypnos.CUDA (grid, listGrid, gridNoBoundary, listGridNoBoundary, gridData, 
              indexC, index1D, index2D, index3D, (!!!), unsafeIndex2D, unsafeIndex1D,
              fun, boundary,
              run, runA
              ) where

import Ypnos hiding (run, fun)
import Ypnos.CUDA.Expr.Fun
import Ypnos.CUDA.Expr.Combinators
