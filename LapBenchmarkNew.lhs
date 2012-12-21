> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> import System.Environment
> import PPM.Image

> import Ypnos
> import Ypnos.Core.Combinators
> import Ypnos.Core.Grid

> laplace2D = [fun| X*Y:| _  t  _ |
>                       | l @c  r |
>                       | _  b  _ | -> t + l + r + b - 4.0*c |]

> zeroBound = [boundary| Double (-1, *j) -> 0.0
>                               (+1, *j) -> 0.0 
>                               (*i, -1) -> 0.0 
>                               (*i, +1) -> 0.0 |]

> go fname =   do -- Read image file
>                 -- Run Ypnos stencil
>                 (x, y, img) <- read_ppm fname
>                 let g = listGrid (X :* Y) (0, 0) (x, y) img zeroBound
>                 let g' = applyConvert laplace2D g
>                 -- Write data to image file
>                 write_ppm (fname++"-ypnos") x y (gridData g')