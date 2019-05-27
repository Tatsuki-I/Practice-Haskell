module Algorithm.Parallel.Sort.ParallelQuickSort where

import           Data.List
import Control.Parallel


import Control.Parallel.Strategies ( runEval
                                   , rpar)

parallelQuickSort          :: Ord a => [a] -> [a]
parallelQuickSort []       =  []
parallelQuickSort (x : xs) =  forceList lesser  `par`
                              forceList greater `par`
                              lesser ++ x : greater
                                where
                                  lesser  = parallelQuickSort [y | y <- xs, y < x]
                                  greater = parallelQuickSort [y | y <- xs, y >= x]

forceList :: [a] -> ()
forceList =  foldr pseq ()


pqsort          :: Ord a => [a] -> [a]
pqsort []       =  []
pqsort (x : xs) =  runEval $ do sls <- (rpar . pqsort) lesser
                                sgs <- (rpar . pqsort) greater
                                return $ sls ++ x : sgs
                                where (lesser, greater) = partition (< x) xs
