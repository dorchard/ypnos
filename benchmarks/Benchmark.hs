{-# LANGUAGE GADTs #-}

module Benchmark where

import System.Environment

import Ypnos.Examples.Stencils
  (runAvgY, runAvg, runAvgY', raiseToList, runLife, runLife')

import Criterion
import Criterion.Monad
import Criterion.Analysis
import Criterion.Environment
import Criterion.Config
import Data.Vector.Unboxed hiding (map, mapM, foldr, foldr1, (++))
import Prelude hiding (sum, length)
import Control.Monad.Trans

l  = [1,2,3,4,5,6,7,8,9,10]
l2 = [True, False, False, True, False, True]

avgGPU = runAvgY l
avgCPU = runAvgY' l
avgA = raiseToList runAvg l

lifeCPU = runLife' l2
lifeGPU = runLife  l2

stenBench :: ((Int,Int) -> b) -> [Benchmark]
stenBench f = [ bench "10x10" $ whnf f (10,10)
              , bench "100x10" $ whnf f (100,10)
              , bench "100x100" $ whnf f (100,100)
              --, bench "1000x1000" $ whnf f (1000,1000)
              ]

--main = defaultMain [ bgroup "Ypnos" (stenBench runAvgY')
--                   , bgroup "Accelerate" (stenBench runAvgA')
--                   ]
runB :: Fun -> Int -> IO Double
runB (Fun f) x = let v = do env <- measureEnvironment
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

type FunS = String
type ImplS = String

data Fun where
  Fun :: ((Int,Int) -> b) -> Fun

getFun :: FunS -> ImplS -> Fun
getFun "avg" "gpu" = Fun avgGPU
getFun "avg" "cpu" = Fun avgCPU
getFun "life" "gpu" = Fun lifeGPU
getFun "life" "cpu" = Fun lifeCPU

main = do [function, impl, begin, step, end, filename] <- getArgs
          let fun = getFun function impl
          let b = read begin :: Int
          let s = read step :: Int
          let e = read end :: Int

          a <- makeSet (runB fun) ([b, s.. e])
          let ca = printCSV a
          writeFile filename ca
