{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Ypnos
import Ypnos.Core.Grid

import System.Environment
import PPM.Image

-- Build
--  ghc --make -O2 LaplaceExample.hs -o LaplaceExample -fcontext-stack=1024

-- The -fcontext-stack parameter is very important

-- Run (outputs boat.512.ypnos.ppm)
-- ./LaplaceExample boat.512 1 

laplace2D = [fun| X*Y:| _  t  _ |
                      | l @c  r |
                      | _  b  _ | -> (t + l + c + r - 4*b) |]

zeroBound = [boundary| Double from (-1, -1) to (+1, +1) -> 0.0 |]

