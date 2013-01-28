import Testing.Ypnos.CUDA.Expr.Combinators (runAvgY, runAvg, raiseToList)
import Criterion
import Criterion.Monad
import Criterion.Environment
import Criterion.Config
import Control.Monad.Reader
import Statistics.Types

l = [1,2,3,4,5,6,7,8,9,10]

runAvgY' = raiseToList runAvgY l
runAvgA' = raiseToList runAvg l

stenBench :: ((Int,Int) -> b) -> [Benchmark]
stenBench f = [ bench "10x10" $ whnf f (10,10)
              , bench "100x10" $ whnf f (100,10)
              , bench "100x100" $ whnf f (100,100)
              --, bench "1000x1000" $ whnf f (1000,1000)
              ]

--main = defaultMain [ bgroup "Ypnos" (stenBench runAvgY')
--                   , bgroup "Accelerate" (stenBench runAvgA')
--                   ] 
main = let v  = do env <- measureEnvironment
                   runBenchmark env (whnf runAvgY' (10,10))
       in       do a <- withConfig defaultConfig v
                   putStrLn "This is it:"
                   print a
    

