-- ##Cuples and tuples##

-- es1
swap :: (Int, Int) -> (Int, Int)
swap (x, y) = (y, x)

-- es2
order :: (Int, Int, Int) -> (Int, Int, Int)
order (x, y, z) | x > y = order (y, x, z)
                | y > z = order (x, z, y)
                | otherwise = (x, y, z)


-- es3
type Complex = (Double, Double)

sumC :: Complex -> Complex -> Complex
sumC (x, y) (w, z) = (x + w, y + z)

negC :: Complex -> Complex
negC (x, y) = (negate x, y)

subC :: Complex -> Complex -> Complex
subC x y = sumC x (negC y)