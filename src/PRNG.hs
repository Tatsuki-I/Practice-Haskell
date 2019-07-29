module PRNG where

import qualified Data.Array.Repa as R
import           Data.Array.Repa.Algorithms.Randomish
import           Control.Monad.ST (runST)

randsDA   :: Int -> R.Array R.U R.DIM1 (Int, Int)
randsDA n =  runST $ do R.computeUnboxedP $ R.zipWith (,) arr1 arr2
           where arr1 =  runST $ do R.computeUnboxedP $ R.map (round . (* 1000)) $ randomishDoubleArray (R.Z R.:. n) 0 1 1
                 arr2 =  runST $ do R.computeUnboxedP $ R.map (round . (* 1000)) $ randomishDoubleArray (R.Z R.:. n) 0 1 2

prot     :: R.Array R.U R.DIM1 (Int, Int) -> R.Array R.U R.DIM2 Int
prot arr =  R.fromListUnboxed (R.Z R.:. 1000 R.:. 1000) $ replicate 1000000 0

mt      :: Int -> [Int]
mt seed =  undefined
