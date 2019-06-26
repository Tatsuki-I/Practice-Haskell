module GoldenRatio where

fib :: Num a => [a]
fib =  1 : 1 : zipWith (+) fib (tail fib)

gr n = y / x
       where (x : y : _) = drop n fib

grs = map (gr) [1 ..]
