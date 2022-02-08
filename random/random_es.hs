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

-- ## es4: tree as input, a couple list containing as first an even number
-- and as second with an odd one and so on ##
evenOddTreeElems :: Integral a => Tree a -> [(a, a)]
evenOddTreeElems (Node x ts) = zip (filter even (x : concatMap elements ts)) (filter odd (x : concatMap elements ts))
  where
    elements (Node x ts) = x : concatMap elements ts

-- ## es5 ##
invAndNull :: [Float] -> ([Float], Int)
invAndNull xs = (filter (not . isNaN) (map (1 /) xs), length (filter isNaN xs))

-- ## es6 ##
evenPosElem :: [a] -> [a]
evenPosElem xs = map fst (filter (even . snd) (zip xs [0 ..]))

-- ## es7 ##
split' :: String -> (String, String)
split' "" = ("", "")
split' ss = aux ss ("", "")
  where
    aux [] (fsts, snds) = (fsts, snds)
    aux (c : cs) (fsts, _)
      | c == ',' = aux "" (fsts, cs)
      | otherwise = aux cs (fsts ++ [c], "")

-- ## es8 ##
count :: String -> Int
count xs = length $ filter (all (\ xs [x] -> x `elem` ['A' .. 'Z'])) (words xs)

-- >>> count ""
-- 0
