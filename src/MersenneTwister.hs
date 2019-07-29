module MersenneTwister where

import Data.Bits        ((.&.), (.|.), xor, shiftR, shiftL)
import Data.Word        (Word32)
import Data.Int         (Int32)
import Control.Monad.ST (runST)
import Data.STRef       (newSTRef, modifySTRef, readSTRef, writeSTRef)
import qualified Data.Array.Repa as R
import Data.Array.ST (newArray, readArray, writeArray, runSTArray)
import qualified Data.Array      as A
import qualified Data.Array.MArray as M

type Seed = Word32

{- Period parameters -}
n         = 624
m         = 397
matrixA   = 0x9908b0df :: Word32 -- constant vector a
upperMask = 0x80000000 :: Word32 -- most significant w-r bits
lowerMask = 0x7fffffff :: Word32 -- least significant r bits

initGenrand   :: Seed -> [Word32]
initGenrand s =  first : f first [1 .. (n - 1)]
                 where f                    :: Word32 -> [Word32] -> [Word32]
                       f _    []            =  []
                       f prev (curr : next) =  new : f new next
                                               where new =  ((1812433253 :: Word32) * (prev `xor` (prev `shiftR` 30)) + curr) .&. (0xffffffff :: Word32)
                       first :: Word32
                       first =  s .&. (0xffffffff :: Word32)

initGenrandRepa   :: Seed -> R.Array R.U R.DIM1 Word32
initGenrandRepa s =  R.fromListUnboxed (R.Z R.:. fromIntegral n) $ initGenrand s

initGenrandArray   :: Seed -> A.Array Word32 Word32
initGenrandArray s =  A.listArray (0, n - 1) (initGenrand s)

initByArray                   :: [Word32] -> Int32 -> [Word32]
initByArray initKey keyLength =  undefined
                                 where i = 1
                                       j = 0
                                       k = if fromIntegral n > keyLength
                                              then n
                                              else fromIntegral keyLength

genrandInt32   :: Seed -> [(Word32, Word32)]
genrandInt32 s =  g 0 $ initGenrandArray (5489 :: Word32)
--map tempering $ A.elems $ g [1 .. n] $ initGenrandArray (5489 :: Word32)

mag01 = [0x0, matrixA] :: [Word32]

g       :: Word32 -> A.Array Word32 Word32 -> [(Word32, Word32)]
g i arr
  | 0 <= i &&
    i < (n - m) = g (i + 1) $ runSTArray $ do arr' <- M.thaw arr
                                              writeArray arr' i $ (arr A.! (i + m)) `xor`
                                                                  (y `shiftR` 1) `xor`
                                                                  (mag01 !! fromIntegral (y .&. (0x1 :: Word32)))
                                              return arr'
  | (n - m) <= i &&
    (i < n - 1) = g (i + 1) $ runSTArray $ do arr' <- M.thaw arr
                                              writeArray arr' i $ (arr A.! (i + (m - n))) `xor`
                                                                  (y `shiftR` 1) `xor`
                                                                  (mag01 !! fromIntegral (y .&. (0x1 :: Word32)))
                                              return arr'

  |  otherwise       = let arr' = runSTArray $ do arr' <- M.thaw arr
                                                  writeArray arr' y $ (arr A.! (m - 1)) `xor`
                                                                      (y `shiftR` 1) `xor`
                                                                      (mag01 !! fromIntegral (y .&. (0x1 :: Word32)))
                                                  return arr'

                       in A.assocs arr' ++ g 0 arr'
  where y :: Word32
        y =  ((arr A.! i)       .&. upperMask) .|.
             ((arr A.! (if i /= (n - 1) then (i + 1) else 0)) .&. lowerMask)
{-
  where y :: Word32
        y =  ((arr A.! i)       .&. upperMask) .|.
             ((arr A.! (if i /= n then i + 1 else 1)) .&. lowerMask)
-}
tempering   :: Word32 -> Word32
tempering x =  runST $ do res  <- newSTRef (x :: Word32)
                          res' <- readSTRef res
                          modifySTRef res (xor  (res' `shiftR` 11))
                          res' <- readSTRef res
                          modifySTRef res (xor ((res' `shiftL`  7) .&. (0x9d2c5680 :: Word32)))
                          res' <- readSTRef res
                          modifySTRef res (xor ((res' `shiftL` 15) .&. (0xefc60000 :: Word32)))
                          res' <- readSTRef res
                          modifySTRef res (xor  (res' `shiftR` 18))
                          readSTRef res

{-
f               :: R.Array R.U R.DIM1 Word32 -> [Word32] -> Word32
f arr (x : xs) =  arr R.! (R.Z R.:. fromIntegral x)
                  where y =  ((arr R.! (R.Z R.:. fromIntegral x))       .&. upperMask) .|.
                             ((arr R.! (R.Z R.:. fromIntegral (x + 1))) .&. lowerMask)
-}
