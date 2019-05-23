module Algorithm.Sort.BucketSort where

import Data.Array.ST
import Data.Array

bsort    :: (Ix a, Foldable t) => t a -> [a]
bsort xs =  concatMap (\x -> replicate (snd x) (fst x)) .
            assocs $ runSTArray $
            do bucket <- newArray (minimum xs, maximum xs) 0
               mapM_ (\i -> (+ 1) <$> readArray bucket i >>=
                            writeArray bucket i)
                     xs
               return bucket
