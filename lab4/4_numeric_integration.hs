-- ##Numeric integration##

-- es1
match :: Eq a => [a] -> [a] -> Bool
match xs ys = any (uncurry (==)) (zip xs ys)

-- es2
adiacenti :: Eq a => [a] -> Bool
adiacenti xs = any (uncurry (==)) (zip xs (tail xs))

-- es3
polinomio :: [Float] -> Float -> Float
polinomio xs c = sum ( map (uncurry (*)) (zip xs (map (c ^) [0..])) )

-- es4
perfetto :: Int -> Bool
perfetto x = ps == x
    where
        ps = sum ( filter (== 0) . (x `mod`) [1..x-1] )

-- es5
ordinata :: Ord a => [a] -> Bool
ordinata as = all (uncurry (<=)) (zip as (tail as))

-- es6
-- The reason behind the fact that, implying the above
-- func ordinata with an empty list as argument ([]), the
-- result us always True, if for the laziness of the zip func.
-- In particular the zip func is "lazy from the right", meaning
-- that a second argument is evalueted only if the first is 
-- different from [], not producing any kind of error.