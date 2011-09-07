{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Ypnos
import Ypnos.Core.Grid

import System.Environment
import PPM.Image

-- Build
--  ghc --make -O2 LaplaceExample.hs -o LaplaceExample 
-- Run (outputs boat.512.ypnos.ppm)
-- ./LaplaceExample boat.512 1 

laplace2D = [fun| X*Y:| _  t  _ |
                      | l @c  r |
                      | _  b  _ | -> (t + l + c + r - 4*b) |]

zeroBound = [boundary| Double from (-1, -1) to (+1, +1) -> 0.0 |]

main = do argv <- getArgs
          if (length argv == 2) then
             do 
               (x, y, img) <- read_ppm (argv!!0)
               let iters = read $ argv!!1
               let g0 = listGrid (Dim X :* Dim Y) (0, 0) (x, y) img zeroBound
               let gn = (iterate (runA laplace2D) g0)!!iters
               let img' = gridData gn
               write_ppm ((argv!!0)++".ypnos") x y img'
            else
                putStrLn "Usage: LaplaceExample input iterations"
