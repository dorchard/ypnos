> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE EmptyDataDecls #-}

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import Ypnos
> import Ypnos.Core.Grid

> import Debug.Trace
> import System.Environment

> import PPM.Image

> enumSpace :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
> enumSpace (x0, y0) (xn, yn) = [y0..(yn-1)] >>= (\y -> [x0..(xn-1)] >>= (\x -> [(x, y)]))

> data Arr i a = Arr (UArray i a) i (i, i)

> mk2DArray :: Int -> Int -> Int -> Int -> Double -> [Double] -> Arr (Int, Int) Double
> mk2DArray bx by x y defa dats = 
>     let topOuter = zip (enumSpace (0, 0) (x+2*bx, by)) (repeat defa)
>         leftOuter = zip (enumSpace (0, by) (bx, y+by)) (repeat defa)
>         rightOuter = zip (enumSpace (x+bx, by) (x+2*bx, y+by)) (repeat defa)
>         bottomOuter = zip (enumSpace (0, y+by) (x+2*bx, y+2*by)) (repeat defa)
>         inner = zip (enumSpace (bx, by) (x+bx, y+by)) dats
>         dats' = topOuter ++ leftOuter ++ rightOuter ++ bottomOuter ++ inner
>     in 
>         Arr (array ((0, 0), (x+2*bx-1, y+2*by-1)) dats')
>             (bx, by)
>             ((bx, by), (x+bx, y+by))

> mk2DArray2 x y dats = 
>     Arr (array ((0, 0), (x-1, y-1)) (zip (enumSpace (0, 0) (x, y)) dats)) (0, 0) ((0, 0), (x, y))

> data Nil
> data Cons x xs

> data Index n where
>     Nil :: Index Nil
>     Cons :: a -> Index as -> Index (Cons a as)
>      

> instance Show (Index Nil) where
>     show Nil = ""

> instance (Show a, Show (Index as)) => Show (Index (Cons a as)) where
>     show (Cons a as) = show a ++ "," ++ show as

> 

> instance Eq (Index Nil) where
>     Nil == Nil = True

> instance (Eq a, Eq (Index as)) => Eq (Index (Cons a as)) where
>     (Cons x xs) == (Cons y ys) = (x == y) && (xs == ys)

> instance Ord (Index Nil) where
>     compare Nil Nil = EQ

> instance (Ord a, Ord (Index as)) => Ord (Index (Cons a as)) where
>     (Cons x xs) <= (Cons y ys) = (x <= y) && (xs <= ys)

> instance Ix (Index Nil) where
>     range (Nil, Nil) = [Nil]
>     index (Nil, Nil) Nil = 0
>     inRange (Nil, Nil) Nil = True

> instance (Ix a, Ix (Index as)) => Ix (Index (Cons a as)) where
>     range (Cons x xs, Cons y ys) = (range (xs, ys)) >>= (\y' -> (range (x, y)) >>=
>                                                         (\x' -> return $ Cons x' y'))
>     inRange (Cons x xs, Cons y ys) (Cons a as) = inRange (x, y) a && (inRange (xs, ys) as)
>     index (Cons x xs, Cons y ys) (Cons a as) = (index (x, y) a)*(rangeSize (xs, ys)) + (index (xs, ys) as)

-- (index (x, y) a) + (rangeSize (x, y))*(index (xs, ys) as)

> type Index2 a = Index (Cons a (Cons a Nil))

> mk2DArray3 :: Index2 Int -> Index2 Int -> Double -> [Double] -> Arr (Index2 Int) Double
> mk2DArray3 (Cons bx (Cons by Nil)) (Cons x (Cons y Nil)) defa dats = 
>     let topOuter = zip (range (Cons 0 (Cons 0 Nil), Cons (x+2*bx-1) (Cons (by-1) Nil))) (repeat defa)
>         leftOuter = zip (range (Cons 0 (Cons by Nil), Cons (bx-1) (Cons (y+by-1) Nil))) (repeat defa)
>         rightOuter = zip (range (Cons (x+bx) (Cons by Nil), Cons (x+2*bx-1) (Cons (y+by-1) Nil))) (repeat defa)
>         bottomOuter = zip (range (Cons 0 (Cons (y+by) Nil), Cons (x+2*bx-1) (Cons (y+2*by-1) Nil))) (repeat defa)
>         inner = zip (range (Cons bx (Cons by Nil), Cons (x+bx-1) (Cons (y+by-1) Nil))) dats
>         dats' = topOuter ++ leftOuter ++ rightOuter ++ bottomOuter ++ inner
>     in 
>         Arr (array (Cons 0 (Cons 0 Nil), Cons (x+2*bx-1) (Cons (y+2*by-1) Nil)) dats')
>             (Cons bx (Cons by Nil))
>             ((Cons bx (Cons by Nil)), (Cons (x+bx) (Cons (y+by) Nil)))

> unMk2DArray :: Arr (Int, Int) Double -> [Double]
> unMk2DArray (Arr arr c ((x, y), (x', y'))) = let arr' = ixmap ((0, 0), (y'-y-1, x'-x-1))
>                                                          (\(xa, ya) -> (ya+y, xa+x)) arr
>                                              in elems arr'

> unMk2DArray3 :: Arr (Index2 Int) Double -> [Double]
> unMk2DArray3 (Arr arr c ((Cons x (Cons y Nil)), (Cons x' (Cons y' Nil)))) = 
>                                              let flip = (\(Cons xa (Cons ya Nil)) -> Cons (ya+y) (Cons (xa+x) Nil))::(Index2 Int -> Index2 Int)
>                                                  arr' = ixmap (Cons 0 (Cons 0 Nil), Cons (y'-y-1) (Cons (x'-x-1) Nil))
>                                                           flip arr
>                                              in elems arr'


> run :: (IArray UArray a) => (Arr (Int, Int) a -> a) -> Arr (Int, Int) a -> Arr (Int, Int) a
> run f (Arr arr c (b1, b2@(xn, yn))) = Arr arr' c (b1, b2)
>                               where
>                                 dats' = map (\c' -> (c', f (Arr arr c' (b1, b2)))) 
>                                                                  (range (b1, (xn-1, yn-1)))
>                                                                  --(enumSpace b1 b2)
>                                 arr' = accum (curry snd) arr dats'

> run2 :: (IArray UArray a) => (Arr (Int, Int) a -> a) -> Arr (Int, Int) a -> Arr (Int, Int) a
> run2 f (Arr arr c (b1, b2@(xn, yn))) = Arr arr' c (b1, b2)
>                                 where
>                                   dats' = map (\c' -> (c', f (Arr arr c' (b1, b2))))
>                                                                  (range (b1, (xn-1, yn-1)))
>                                                                  --(enumSpace b1 b2)
>                                   arr' = array (b1, b2) dats'

> run3 :: (IArray UArray a) => (Arr (Index2 Int) a -> a) -> Arr (Index2 Int) a -> Arr (Index2 Int) a
> run3 f (Arr arr c b@(Cons xb1 (Cons yb1 Nil), Cons xb2 (Cons yb2 Nil))) = Arr arr' c b
>                                               where
>                                                 (b1, b2) = b
>                                                 dats' = map (\c' -> (c', f (Arr arr c' b)))
>                                                                  (range (b1, (Cons (xb2-1) (Cons (yb2-1) Nil))))
>                                                 arr' = accum (curry snd) arr dats'

> {-# INLINE unsafeIndex #-}
> unsafeIndex arr i = unsafeAt arr (GHCArr.unsafeIndex (bounds arr) i)

> laplaceU :: Arr (Int, Int) Double -> Double
> laplaceU (Arr arr (i, j) _) =   arr `unsafeIndex` (i-1, j)
>                              + arr `unsafeIndex` (i+1, j)
>                              + arr `unsafeIndex` (i, j+1)
>                              + arr `unsafeIndex` (i, j-1)
>                              - 4*(arr `unsafeIndex` (i, j))

> laplace :: Arr (Int, Int) Double -> Double
> laplace (Arr arr (i, j) _) =  arr!(i-1, j)
>                              + arr!(i+1, j)
>                              + arr!(i, j+1)
>                              + arr!(i, j-1)
>                              - 4*(arr!(i, j))

> laplace2 :: Arr (Int, Int) Double -> Double
> laplace2 (Arr arr (i, j) ((x0,y0), (xn,yn))) = if (i>x0 && i<(xn-1) && j>y0 && j<(yn-1)) then
>                                                   arr!(i-1, j)
>                                                 + arr!(i+1, j)
>                                                 + arr!(i, j+1)
>                                                 + arr!(i, j-1)
>                                                 - 4*(arr!(i, j))
>                                               else
>                                                   0.0

> laplace3 :: Arr (Index2 Int) Double -> Double
> laplace3 (Arr arr (Cons i (Cons j Nil)) _) =   arr!(Cons (i-1) (Cons j Nil))
>                                              + arr!(Cons (i+1) (Cons j Nil))
>                                              + arr!(Cons i (Cons (j+1) Nil))
>                                              + arr!(Cons i (Cons (j-1) Nil))
>                                              - 4*(arr!(Cons i (Cons j Nil)))


> identity :: Arr (Int, Int) Double -> Double
> identity (Arr arr (i, j) _) = arr!(i, j)

> one :: Int -> Int -> Int -> [Double] -> [Double]
> one x y iter img =
>       let arr = mk2DArray 1 1 x y 0.0 img
>           arr' = (iterate (run laplace) arr)!!iter
>           img' = unMk2DArray arr'
>       in
>         img'

> oneU :: Int -> Int -> Int -> [Double] -> [Double]
> oneU x y iter img =
>       let arr = mk2DArray 1 1 x y 0.0 img
>           arr' = (iterate (run laplaceU) arr)!!iter
>           img' = unMk2DArray arr'
>       in
>         img'

> two :: Int -> Int -> Int -> [Double] -> [Double]
> two x y iter img = 
>       let arr = mk2DArray2 x y img
>           arr' = (iterate (run2 laplace2) arr)!!iter
>           img' = unMk2DArray arr'
>       in 
>         img'

> three :: Int -> Int -> Int -> [Double] -> [Double]
> three x y iter img = 
>       let arr = mk2DArray3 (Cons 1 (Cons 1 Nil)) (Cons x (Cons y Nil)) 0.0 img
>           arr' = (iterate (run3 laplace3) arr)!!iter
>           img' = unMk2DArray3 arr'
>       in
>         img'

> rect = do (x, y, img) <- read_ppm "rect"
>           let img' = one x y 10 img
>           write_ppm "rectBench" x y img'

> main = do (x, y, img) <- read_ppm "boat"
>           argv <- getArgs
>           if ((length argv) > 1) then
>             do
>               let img' = case (argv!!0) of "one" -> one x y (read $ argv!!1) img
>                                            "oneU" -> oneU x y (read $ argv!!1) img
>                                            "two" -> two x y (read $ argv!!1) img
>                                            "three" -> three x y (read $ argv!!1) img
>               write_ppm ("boat"++(argv!!0)) x y img'
>             else
>                 putStr "Usage: LapBenchmark one|two|three iters"