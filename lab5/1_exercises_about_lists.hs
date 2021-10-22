-- ##Exercises about lists##

-- es1
lastSumRic :: (Num a, Eq a) => [a] -> Bool
lastSumRic = aux 0
    where
        aux summ [x] = summ == x
        aux summ (x : xs) = aux (summ + x) xs

-- es2
lastSum :: (Num a, Eq a) => [a] -> Bool
lastSum xs = head ys == sum (tail ys)
    where
        ys = reverse xs

-- es3
maxLengthLists :: [[a]] -> [[a]]
maxLengthLists xs = filter ((== maxl) . length) xs 
    where
        maxl = maximum (map length xs)

-- es4
map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

-- es5
filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr aux []
    where
        aux x : xs | f x = x : xs
                   | otherwise = xs 
