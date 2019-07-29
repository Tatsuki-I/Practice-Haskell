{-# Language TypeApplications #-}

module Pi where

import           Data.List (replicate)
import qualified Data.Array.Repa as R
import           System.Random.MWC
import           Control.Monad (forM)
import           Data.Ratio ((%))
import           Control.Parallel.Strategies ( runEval
                                              , rpar)
import           Data.Array.Repa.Algorithms.Randomish

--pi   :: (Num a, Enum a) => a -> IO Int
pi n =  do xs <- randPosList n
           return $ fromRational $ fromIntegral ((sum xs) * 4) % n

lessThan1   :: Double -> Int
lessThan1 x =  if x < 1
                  then 1
                  else 0

dist     :: Double -> Double -> Double
dist x y =  sqrt $ (x ^ 2) + (y ^ 2)

randPosList   :: Integer -> IO [Int]
randPosList n =  do gen <- createSystemRandom
                    forM [1 .. n] (\_ -> randPos gen)


{-
randPosRepa   :: Int -> GenIO -> R.Array R.U R.DIM1 Int
randPosRepa n gen =  R.computeUnboxedP $ R.map (\_ -> randPos gen) arr
                     where arr = R.fromListUnboxed (R.Z R.:. n) $ replicate n 1

--randPosRepa   :: GenIO -> R.Array R.U R.DIM1 Int
--randPosRepa gen =  R.fromListUnboxed (R.Z R.:. 10) [1 .. 10] $ repeat 1

--randPosreRepa gen =  R.computeUnboxedP $ R.map (\_ -> randPos gen) $ R.fromListUnboxed (R.Z R.:. 10000000) $ repeat 1

f = do gen <- createSystemRandom
       return $ randPosRepa 100 gen

rands   :: R.Array R.U R.DIM1 Double
rands =  randomishDoubleArray (R.Z R.:. 10000000) 0 1 1
-}

randPos     :: GenIO -> IO Int
randPos gen =  do x <- uniform @Double gen
                  y <- uniform @Double gen
                  (return . lessThan1) (dist x y)

{-
randPosList n =  forM [1 .. n] (\_ -> randPos)
                 where randPos =  do gen <- createSystemRandom
                                     x <- uniform @Double gen
                                     y <- uniform @Double gen
                                     return (x, y)
-}
-- 
-- randPos'     :: GenIO -> IO (Double, Double)
-- randPos' gen =  runEval $ do x <- (rpar (uniform @Double gen))
--                              y <- (rpar (uniform @Double gen))
--                              return (x, y)
