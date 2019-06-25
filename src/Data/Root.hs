module Data.Root where

import Data.Ratio
import Data.List       (group, partition)
import Data.List.Split (chunksOf)

data Rt = !Int :-/ !Integer
          deriving ( Eq )

data Root = Rational :* Rt
            deriving ( Eq )

instance Show Rt where
    show (a :-/ b) = show a ++ "-/ " ++ show b

instance Show Root where
    show (a :* b) = "(" ++ show a ++ ") * " ++ show b


(-/)     :: Int -> Integer -> Root
(-/) a b =  toRational n :* (a :-/ nb)
            where (n, nb) = (product $ map head fs2, product $ concat (ts2 ++ ts1))
                             where (fs1, ts1) = partition (\xs -> length xs >= a)
                                                          $ (group . factorisation) b
                                   (fs2, ts2) = partition (\xs -> length xs == a)
                                                          $ concatMap (chunksOf a) fs1

factorisation   :: Integer -> [Integer]
factorisation 1 =  []
factorisation x =  v : factorisation (x `div` v)
  where
      v = factors x !! 1
      factors :: Integer -> [Integer]
      factors n = [x | x <- [1..n], n `mod` x == 0]
