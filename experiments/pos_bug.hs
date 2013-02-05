{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
import Ypnos
import Ypnos.Core.Grid

gx g = fst (size g) 
gy g = snd (size g)

clamp = [boundary| Float  (*i, -1) g -> g!!!(i, 0) -- top
                          (-1, *j) g -> g!!!(0, j) -- left
                          (+1, *j) g -> g!!!(gx g, j) -- right
                          (*i, +1) g -> g!!!(i, gy g)
                          (-1, -1) g -> g!!!(0, 0) -- top corners
                          (+1, -1) g -> g!!!(gx g, 0) -- top corners
                          (-1, +1) g -> g!!!(0, gy g) -- top corners
                          (+1, +1) g -> g!!!(gx g, gy g) |]

clamp2 = [boundary| Float  (*i, -1) g -> g!!!(i, 0) -- top
                          (*i, +1) g -> g!!!(i, gy g)
                          (-1, -1) g -> g!!!(0, 0) -- top corners
                          (+1, -1) g -> g!!!(gx g, 0) -- top corners
                          (-1, +1) g -> g!!!(0, gy g) -- top corners
                          (+1, +1) g -> g!!!(gx g, gy g) |]

-- This produces the following error when printed: array *** Exception: Error
-- in array index
xs = listGrid (Dim X :* Dim Y) (0, 0) (2,1) [21.0,2.0] clamp 

-- This print just fine.
xs' = listGrid (Dim X :* Dim Y) (0, 0) (2,1) [21.0,2.0] clamp2
