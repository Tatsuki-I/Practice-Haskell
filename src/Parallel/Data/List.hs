module Parallel.Data.List where

import Control.Parallel.Strategies

{-
replicate      :: Int -> a -> [a]
replicate n  x =  do left  <- rpar (take d (repeat x))
                     right <- rpar (take (d + m) $ repeat x)
                     return left ++ right
                     where (d, m) = divMod n 2
-}

-- replicate n  x =  (take d (repeat x)) ++ (take (d + m) $ repeat x)
--                   where (d, m) = divMod n 2

-- testReplicate = Parallel.Data.List.replicate 10 1
