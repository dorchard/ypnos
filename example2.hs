{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Example2 where

import Ypnos
import Ypnos.Core.Grid

ave2D = [fun| X*Y:| a  b  c |
                  | d @e  f |
                  | g  h  i | -> (a+b+c+d+e+f+g+h+i)/9.0 |]

boundary2D = [boundary| Double from (-1, -1) to (+1, +1) -> 0.0 |]


gridDat = [1.0,4.0,7.0,2.0,5.0,8.0,3.0,6.0,9.0]::[Double]
grid1 = listGridWithBoundaries (Dim X :* Dim Y) (0, 0) (3, 3) gridDat boundary2D

grid1' = run ave2D grid1
grid1'' = runA ave2D grid1

-- Expected
-- Grid <X> * <Y> * <> cursor = Ix[0,0] bounds = (Ix[0,0], Ix[2,2])
-- elems = [(Ix[0,0],1.3333333333333333),(Ix[0,1],2.3333333333333335),(Ix[0,2],1.7777777777777777),
--          (Ix[1,0],3.0),(Ix[1,1],5.0),(Ix[1,2],3.6666666666666665),
--          (Ix[2,0],2.6666666666666665), (Ix[2,1],4.333333333333333),(Ix[2,2],3.111111111111111)]
