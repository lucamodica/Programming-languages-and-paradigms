lucaRec :: Integral a => [a] -> [(a, a)]
lucaRec [] = []
lucaRec xs = aux xs 0
  where
    aux [] _ = []
    aux (x : xs) v = (x, v + x) : aux xs (v + x)

luca :: Integral a => [a] -> [(a, a)]
luca [] = []
luca (x : xs) = scanl (\(a, b) c -> (c, b + c)) (x, x) xs
