module Sieve where

import Data.Array.ST
import Data.Array
import Control.Monad

sieve               :: (Integral a, Ix a) => a -> [a]
sieve n
    | n < 2     =  []
    | otherwise =  map fst . filter snd . (drop 2 . assocs) $ runSTArray $
                   do a <- newArray (0, n) True
                      writeArray a 0 False
                      writeArray a 1 False
                      mapM_ (\i -> writeArray a i False) [4, 6 .. n]
                      mapM_ (\i -> readArray a i >>= 
                                   flip when (mapM_ (\j -> writeArray a j False)
                                                    [i * i, i * (i + 2) .. n])) 
                            [3, 5 .. sqn]
                      return a
                      where sqn :: (Integral a) => a
                            sqn =  (ceiling . sqrt . fromIntegral) n
