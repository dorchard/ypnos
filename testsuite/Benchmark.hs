import Testing.Ypnos.CUDA.Expr.Combinators (runAvgY, runAvg, raiseToList)
import Criterion
import Criterion.Monad
import Criterion.Environment
import Criterion.Config
import Data.Vector.Unboxed hiding (map, mapM, foldr, foldr1, (++))
import Prelude hiding (sum, length)

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
runB :: ((Int,Int) -> b) -> Int -> IO Double
runB f x = let v = do env <- measureEnvironment
                      l <- runBenchmark env (whnf f (x,x))
                      s <- liftIO $ analyseSample 0.5 l 5
                      return (

         in  withConfig defaultConfig v

makeSet :: (Monad m, Show a) => (Int -> m a) -> m [[String]]
makeSet f = let l = [1, 20..100]
                tup x = do y <- f x
                           return [show x,show y]
            in  mapM tup l

insert i x y = x ++ i ++ y

makeLine [] = ""
makeLine xs = foldr1 (insert ", ") xs

printCSV :: [[String]] -> String
printCSV lls = foldr (insert "\n") "" (map makeLine lls)

main = do a <- makeSet (runB runAvgY')
          b <- makeSet (runB runAvgA')
          let ca = printCSV a
          let cb = printCSV b
          writeFile "ypnos.csv" ca
          writeFile "accelerate.csv" cb
    

