-- useful data structures
data Tree a = Node a [Tree a]

-- ## es1: list as input, list of couple with element and the
-- sum of itself and the list previous elements ##
-- Recursive
sumPrevRec :: Integral a => [a] -> [(a, a)]
sumPrevRec [] = []
sumPrevRec xs = aux xs 0
  where
    aux [] _ = []
    aux (x : xs) v = (x, v + x) : aux xs (v + x)

-- With prelude funcs
sumPrev :: Integral a => [a] -> [(a, a)]
sumPrev [] = []
sumPrev (x : xs) = scanl (\(a, b) c -> (c, b + c)) (x, x) xs

-- ## es2 ##
-- Recursive
mediaRec :: [(Int, Int)] -> Int
mediaRec [] = 0
mediaRec xs = aux 0 0 xs
  where
    aux sum len []
      | sum == 0 || len == 0 = 0
      | otherwise = div sum len
    aux sum len ((x, y) : xs)
      | x >= 18 = aux (sum + y) (len + 1) xs
      | otherwise = aux sum len xs

-- With prelude funcs
media :: [(Int, Int)] -> Int
media = (\x -> div (sum x) (length x)) . map snd . filter ((>= 18) . fst)

-- ## es3 ##
maxt :: Ord a => Tree a -> a
maxt (Node x ts) = maximum (x : concatMap elements ts)
  where
    elements (Node x ts) = x : concatMap elements ts

-- >>> maxt (Node 3 [Node 9 []])
