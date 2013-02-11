module Ypnos.CUDA (grid, listGrid, gridNoBoundary, listGridNoBoundary,
              gridData, indexC, index1D, index2D, index3D, (!!!), size,
              unsafeIndex2D, unsafeIndex1D, fun, funGPU, boundary, run, runA,
              runG, reduceG, mkReducer, Fun1(..), Fun2(..), Sten(..)) where

import Ypnos 
import Ypnos.CUDA.Expr.Fun hiding (size)
import Ypnos.CUDA.Expr.Combinators
