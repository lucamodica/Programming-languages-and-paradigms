-- ##Summary exercises##

-- #List Sets#
--es1
union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union xs [] = xs
union [] ys = ys
union (x : xs) (y : ys) | x == y    = x : union xs ys
                        | x < y     = x : union xs (y : ys)
                        | otherwise = y : union (x : xs) ys

-- es2
intersection :: Ord a => [a] -> [a] -> [a]
intersection [] [] = []
intersection xs [] = []
intersection [] ys = []
intersection (x : xs) (y : ys) | x == y    = x : intersection xs ys
                               | x < y     = intersection xs (y : ys)
                               | otherwise = intersection (x : xs) ys

-- es3
difference :: Ord a => [a] -> [a] -> [a]
difference [] [] = []
difference xs [] = xs
difference [] ys = []
difference (x : xs) (y : ys) | x == y    = difference xs ys
                             | x < y     = difference xs (y : ys)
                             | otherwise = x : difference (x : xs) ys