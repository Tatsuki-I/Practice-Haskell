module RandomTest where

import           System.Random.MWC
import           Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as VU

--f :: Int
f seed =  runST $ do gen <- initialize $ VU.fromList seed
                     r <- uniform gen
                     return (r :: Int)

