module DiagonalisingListComprehensions where

--dlc       :: [a] -> [a] -> [a]
dlc xs ys =  concatMap (uncurry zip) $ zip (f (length ys) xs) $ map reverse $ f (length xs) ys

f      :: Int -> [a] -> [[a]]
f n xs =  map (`take` xs) [1 .. n] ++ (if length xs < n
                                          then replicate (length xs - n) xs
                                          else []) ++ g n xs

g            :: Int -> [a] -> [[a]]
g _ [x]      =  []
g n (x : xs) =  take n xs : g n xs
