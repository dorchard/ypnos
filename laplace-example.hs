{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

import System.Environment
import PPM.Image

import Ypnos
import Ypnos.Core.Grid

laplace2D = [fun| X*Y:| _  t  _ |
                      | l @c  r |
                      | _  b  _ | -> t + l + r + b - 4.0*c |]

zeroBound = [boundary| Double (-1, *j) -> 0.0
                              (+1, *j) -> 0.0 
                              (*i, -1) -> 0.0 
                              (*i, +1) -> 0.0 |]

main = do -- Read image file
        argv <- getArgs
        (x, y, img) <- read_ppm (argv!!0)
        -- Run Ypnos stencil
        let g = listGrid (Dim X :* Dim Y) (0, 0) (x, y) img zeroBound
        let g' = runA laplace2D g
        -- Write data to image file
        write_ppm ((argv!!0)++"-ypnos") x y (gridData g')
          