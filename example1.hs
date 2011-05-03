{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Example1 where

import Ypnos
import Ypnos.Core.Grid

import Prelude hiding (iterate)

dat = [1,2,3]::[Int]

grid1 = listGrid (Dim X) 0 3 dat

f = [fun| X:| a @b c | -> a+b+c |]

--grid1' = run f (defaults grid1 0)

--converge = reducer (+) (+) 0 (\x -> x > 100)

--grid1'' = iterate f converge (defaults grid1 0)

-- result show be elems = [(Ix[0],57),(Ix[1],82),(Ix[2],59)]

