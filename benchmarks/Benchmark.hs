module Benchmark where

import System.Environment

import Ypnos.Examples.Stencils (runAvgY, runAvg, runAvgY', raiseToList)

import Criterion
import Criterion.Monad
import Criterion.Analysis
import Criterion.Environment
import Criterion.Config
import Data.Vector.Unboxed hiding (map, mapM, foldr, foldr1, (++))
import Prelude hiding (sum, length)
import Control.Monad.Trans

l = [1,2,3,4,5,6,7,8,9,10]

avgY = raiseToList runAvgY l
avgYorig = runAvgY' l
avgA = raiseToList runAvg l

stenBench :: ((Int,Int) -> b) -> [Benchmark]
stenBench f = [ bench "10x10" $ whnf f (10,10)
              , bench "100x10" $ whnf f (100,10)
              , bench "100x100" $ whnf f (100,100)
              --, bench "1000x1000" $ whnf f (1000,1000)
              ]

--main = defaultMain [ bgroup "Ypnos" (stenBench runAvgY')
--                   , bgroup "Accelerate" (stenBench runAvgA')
--                   ]
runB :: ((Int,Int) -> b) -> Int -> IO Double
runB f x = let v = do env <- measureEnvironment
                      l <- runBenchmark env (whnf f (x,x))
                      s <- liftIO $ analyseSample 0.5 l 5
                      m <- analyseMean l 5
                      return m

         in  withConfig defaultConfig v

makeSet :: (Monad m, Show a) => (Int -> m a) -> [Int] -> m [[String]]
makeSet f range = let tup x = do y <- f x
                                 return [show x,show y]
                  in  mapM tup range

insert i x y = x ++ i ++ y

makeLine [] = ""
makeLine xs = foldr1 (insert ", ") xs

printCSV :: [[String]] -> String
printCSV lls = foldr (insert "\n") "" (map makeLine lls)

main = do [function, impl, begin, step, end, filename] <- getArgs
          let fun = case function of
                      "avg" -> case impl of
                                 "gpu" -> avgY
                                 "cpu" -> avgYorig
                                 _ -> error ("Not a know implementation.")
                      _ -> error ("Not a know function.")
          let b = read begin :: Int
          let s = read step :: Int
          let e = read end :: Int

          a <- makeSet (runB fun) ([b, s.. e])
          let ca = printCSV a
          writeFile filename ca
