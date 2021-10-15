-- ##Logarithmic fibonacci##

type Matrix = (Integer, Integer, Integer, Integer)

mul :: Matrix -> Matrix -> Matrix
mul (a11, a12, a21, a22) (b11, b12, b21, b22) = 
    (a11 * b11 + a12 * b21,
     a11 * b12 + a12 * b22,
     a21 * b11 + a22 * b21,
     a21 * b12 + a22 * b22)

pow :: Matrix -> Int -> Matrix
pow a k | k == 0         = (1, 0, 0, 1)
        | k `mod` 2 == 0 = b `mul` b
        | otherwise      = a `mul` b `mul` b
    where
        b = a `pow` (k `div` 2)

fibonacci :: Int -> Integer
fibonacci k = result
    where
        (_, result, _, _) = (1, 1, 1, 0) `pow` k
