{-# LANGUAGE FlexibleContexts #-}
module Dot (dotp') where 

import Prelude hiding (zipWith)
import Data.Array.Accelerate hiding (flatten)
import Data.Array.Accelerate.Interpreter 

dotp :: Vector Float -> Vector Float -> Acc (Scalar Float)
dotp xs ys = let xs' = use xs
                 ys' = use ys
             in 
             fold (+) 0 (zipWith (*) xs' ys')

flatten :: Scalar t -> t
flatten x = toList (x) !! 0

dotp' :: Int -> [Float] -> [Float] -> Float
dotp' len xs ys =  let xs' = fromList (Z :. len) xs :: Vector Float
                       ys' = fromList (Z :. len) ys :: Vector Float
                   in
                   flatten $ run $ dotp xs' ys'
