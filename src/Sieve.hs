module Sieve where

import Data.Array.ST
import Control.Monad

--sieve   :: Int -> MArray Int Bool
sieve n =  runSTArray $ do arr <- newArray (0, n) True
                           writeArray arr 0 False
                           writeArray arr 1 False
                           mapM_ (\i -> writeArray arr i False) [4, 6 .. n]
                           mapM_ (\i -> readArray arr i >>= 
                                        (flip when) (mapM_ (\j -> (writeArray arr j False))
                                                           [i * i, i * (i + 2) .. n])) 
                                 [3, 5 .. n]
                           return arr
                           --where sqn = (fromIntegral . ceiling . sqrt) n

