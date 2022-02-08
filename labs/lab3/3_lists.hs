-- ##Lists##

-- es1
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

-- es2
[1, 2 + 3] -- well typed
[True, 2 == 3] -- well typed
[1, True] -- bad typed
[[1], []] -- well typed
[[1], False] -- bad typed
1 : 2 -- well typed
[[], [[2.5, 3], 4 : 5 : []]] -- well typed

-- es3
mean :: [Int] -> Float
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- es4
factorial :: Int -> Int
factorial x = product [2..x]

-- es5
range :: Int -> Int -> [Int]
range x y | x <= y = [x] ++ range (x + 1) y
          | otherwise = []


-- es6
prime :: Int -> Bool
prime n = aux 2
  where
    aux k | k >= n         = k == n
          | n `mod` k == 0 = False
          | otherwise      = aux (k + 1)

listPrime :: Int -> [Int]
listPrime xs = aux 2
    where
        aux n | n > xs = []
              | prime n = n : aux (n + 1)
              | otherwise = aux (n + 1)