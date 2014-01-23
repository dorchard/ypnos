{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Ypnos
import Ypnos.Core.Grid

import System.Environment
import PPM.Image

-- Build [note the context-stack needs increasing as the types are so big]
--  ghc --make -O2 LapGaussianExample.hs -o LapGaussianExample -fcontext-stack=1024
-- Run (outputs boat.512.ypnos.ppm)
-- ./LapGaussianExample boat.512 1 

laplaceOfGaussian2D =
            [fun| X*Y:| _  _  a   _  _ |
                      | _  b  c   d  _ |
                      | e  f  @g  h  i |
                      | _  j  k   l  _ |
                      | _  _  m   _  _ | -> 0-a-b-2*c-d-e-2*f+16*g-2*h-i-j-2*k-l-m |]

zeroBound = [boundary| Double from (-2, -2) to (+2, +2) -> 0.0 |]

main = do argv <- getArgs
          if (length argv == 2) then
             do 
               (x, y, img) <- read_ppm (argv!!0)
               let iters = read $ argv!!1
               let g0 = listGrid (Dim X :* Dim Y) (0, 0) (x, y) img zeroBound
               let gn = (iterate (runA laplaceOfGaussian2D) g0)!!iters
               let img' = gridData gn
               write_ppm ((argv!!0)++".ypnos") x y img'
            else
                putStrLn "Usage: LapGaussianExample input iterations"
