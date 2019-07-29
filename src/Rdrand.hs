{-# LANGUAGE ForeignFunctionInterface #-}

module Rdrand where

import Foreign.C.Types
import Foreign.Ptr ( Ptr
                   , intPtrToPtr)

foreign import ccall "sin"
  c_sin :: Double -> Double

foreign import ccall "immintrin.h _rdrand_u32_step"
  rdrand_u32_step :: Ptr CInt -> IO CInt

f x = c_sin x

{-
rdrand :: IO CInt
rdrand =  rdrand_u32_step (a)
          where a = (0 :: Ptr CInt)
                -}
