-- ##Numeric integration##

-- es1
match :: Eq a => [a] -> [a] -> Bool
match xs ys = any (uncurry (==)) (zip xs ys)

-- es2
adiacenti :: Eq a => [a] -> Bool
adiacenti xs = any (uncurry (==)) (zip xs (tail xs))