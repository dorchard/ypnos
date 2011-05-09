{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Example1 where

import Ypnos
import Ypnos.Core.Grid

dat = [1,2,3]::[Int]

datBound = [boundary| Int -1 -> 0
                          +1 -> 0 |]

grid1 = listGridWithBoundaries (Dim X) 0 3 dat datBound

f = [fun| X:| a @b c | -> a+b+c |]

grid1' = run f grid1

