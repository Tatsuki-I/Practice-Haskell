module Parallel.ParallelTest where

import Control.Parallel.Strategies ( runEval
                                   , rpar)
fact   :: Integral a => a -> a
fact 0 =  0
fact 1 =  1
fact n =  n * fact (n - 1)

pFact   :: Integral a => a -> a
pFact 0 =  0
pFact 1 =  1
pFact n =  runEval $ do next <- (rpar . pFact) (n - 1)
                        (return . (n *) . pFact) next


