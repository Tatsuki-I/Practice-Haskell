module DiagonalisingListComprehensions where

--dlc       :: [a] -> [a] -> [a]
dlc xs ys =  concatMap (uncurry zip) $ zip (f (length ys) xs) $ map reverse $ f (length xs) ys
--dlc xs ys =  concatMap (uncurry zip) $ zip (f ln xs) $ map reverse (f ln ys)
             where ln =  min (length xs) (length ys)

f      :: Int -> [a] -> [[a]]
f n xs =  map (`take` xs) [1 .. n] ++ (if length xs < n
                                          then replicate (length xs - n) xs
                                          else []) ++ g n xs
          where g            :: Int -> [a] -> [[a]]
                g _ [x]      =  []
                g n (x : xs) =  take n xs : g n xs
