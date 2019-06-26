module Data.Root where

import Data.Ratio      ((%), numerator)
import Data.List       (group, partition)
import Data.List.Split (chunksOf)

data Rt = !Int :-/ !Integer
          deriving ( Eq )

data Root' = Rational :* Rt
             deriving ( Eq )

data Root = Zero
          | !Root' :+ !Root
            deriving ( Eq )

instance Show Rt where
    show (1 :-/ 1) = "1"
    show (2 :-/ b) = "-/ " ++ show b
    show (a :-/ b) = show a ++ "-/ " ++ show b

instance Show Root' where
    show (a :* (1 :-/ _)) = "(" ++ show a ++ ")"
    show (a :* b) = "(" ++ show a ++ ") * " ++ show b

instance Show Root where
    show Zero = "Zero"
    show (a :+ b) = f a b
                    where f x Zero     = show x
                          f x (y :+ z) = show x ++ " + " ++ f y z

instance Num Root where
    a    + Zero     =  a
    Zero + a        =  a
    a    + (b :+ rs) = f a b + rs
                      where f             :: Root -> Root' -> Root
                            f Zero z      =  z :+ Zero
                            f (rt1@(c1 :* r1) :+ rs) rt2@(c2 :* r2)
                              | r1 == r2  =  ((c1 + c2) :* r1) :+ rs
                              | otherwise =  rt1 :+ f rs rt2

    _    * Zero     =  Zero
    Zero * _        =  Zero
    a    * (r :+ rs) = f a r + a * rs
                      where f :: Root -> Root' -> Root
                            f Zero z      =  Zero
                            f (a :+ b) c  =  g a c + f b c
                            g :: Root' -> Root' -> Root
                            g (c1 :* (n1 :-/ r1))
                              (c2 :* (n2 :-/ r2)) | n1 == n2 =  n1 -/ rt
                                                                where rt :: Rational
                                                                      rt =  product $ map toRational [r1 , r2 ] ++ replicate n1 c2 ++ replicate n1 c2
                                                                      n  =  fromIntegral n1

    negate Zero             = Zero
    negate ((c :* r) :+ rs) = ((- c) :* r) :+ negate rs

(-/)     :: Int -> Rational -> Root
a -/ b =  (toRational n :* (na :-/ nb)) :+ Zero
          where b' = numerator b
                na = if nb == 1
                        then 1
                        else a
                (n, nb) = (product $ map head fs2, product $ concat (ts2 ++ ts1))
                           where (fs1, ts1) = partition (\xs -> length xs >= a)
                                                        $ (group . factorisation) b'
                                 (fs2, ts2) = partition (\xs -> length xs == a)
                                                        $ concatMap (chunksOf a) fs1
                factorisation   :: Integer -> [Integer]
                factorisation 1 =  []
                factorisation x =  v : factorisation (x `div` v)
                                   where v = factors x !! 1
                                         factors :: Integer -> [Integer]
                                         factors n = [x | x <- [1..n], n `mod` x == 0]

