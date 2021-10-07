-- ##From iterative ro recursive##

--es1
factorial :: Int -> Int
factorial = aux 1
    where
        aux res 0 = res
        aux res n = aux (res * n) (n - 1)

--es2
bits :: Int -> Int
bits = aux 0
    where
        aux bits 0 = bits
        aux bits n = aux (bits + mod n 2) (div n  2)

--es3
euclid :: Int -> Int -> Int
euclid n m  | m == n = n
            | m < n = euclid (n - m) m
            | otherwise = euclid n (m - n)