-- useful data structures
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use even" #-}
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
count xs = length $ filter aux (words xs)
  where
    aux = all (\x -> x `elem` ['A' .. 'Z'])

-- ## es9: 2 strings as input, returs a boolean to
-- tell if the second string is inside the first one
-- (recursive)
isSubstring :: String -> String -> Bool
isSubstring [] [] = True
isSubstring _ [] = True
isSubstring [] _ = False
isSubstring xs ys = aux xs ys 0 0
  where
    aux xs ys ix iy
      | lengthh ys == iy = True
      | lengthh xs == ix = False
      | (xs !! ix) == (ys !! iy) = aux xs ys (ix + 1) (iy + 1)
      | (xs !! ix) /= (ys !! iy) = aux xs ys (ix + 1) 0
      | otherwise = False
    lengthh [] = 0
    lengthh (x : xs) = 1 + lengthh xs

-- ## es10: given an integer n and a value v returns a
-- list of length n initialized with v, namely all elements
-- are equal to v ##
-- Recursive
myReplicateR :: Int -> a -> [a]
myReplicateR 0 _ = []
myReplicateR n v = v : myReplicateR (n - 1) v

-- With prelude funcs
myReplicateC :: Int -> a -> [a]
myReplicateC = replicate

-- ## es11 given a list of integers computes the sum of
-- the values that are odd ##
-- Recursive
sumOddR :: [Int] -> Int
sumOddR [] = 0
sumOddR (x : xs)
  | mod x 2 == 0 = sumOddR xs
  | otherwise = x + sumOddR xs

-- With prelude funcs
sumOddC :: [Int] -> Int
sumOddC = sum . filter odd

-- ## es12: given a list xs and a integer n returns a list
-- containing the elements that is replicated at least n times
-- in xs ##
-- Recursive
replR :: Eq a => [a] -> Int -> [a]
replR [] _ = []
replR _ 0 = []
replR xs n = auxAux xs n []
  where
    auxAux [] _ fs = fs
    auxAux (x : xs) n fs
      | not (isIn fs x) && aux (x : xs) (replAux x n) x n = auxAux xs n (x : fs)
      | otherwise = auxAux xs n fs
    aux [] [] _ _ = True
    aux [] _ _ _ = False
    aux _ [] _ _ = True
    aux (x : xs) (r : rs) e n
      | x == r = aux xs rs e n
      | otherwise = aux xs (replAux e n) e n
    isIn [] _ = False
    isIn (x : xs) e
      | x == e = True
      | otherwise = isIn xs e
    replAux _ 0 = []
    replAux x n = x : replAux x (n - 1)

-- With prelude funcs
-- TO DO

-- ## es13: given a list of strings xs returns the total
-- number of vowels in strings that are palindromes ##
-- Recursive
countVowelPaliR :: [String] -> Int
countVowelPaliR [] = 0
countVowelPaliR (x : xs)
  | x == reversee x = countVowel x + countVowelPaliR xs
  | otherwise = countVowelPaliR xs
  where
    reversee [] = []
    reversee (x : xs) = reversee xs ++ [x]
    countVowel [] = 0
    countVowel (x : xs)
      | x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u' = 1 + countVowel xs
      | otherwise = countVowel xs

-- With prelude funcs
countVowelPaliC :: [String] -> Int
countVowelPaliC xss = sum $ map (length . filterVowels) (filter (\xs -> xs == reverse xs) xss)
  where
    filterVowels = filter (\x -> x `elem` ['a', 'e', 'i', 'o', 'u'])

-- >>> countVowelPaliC ["ciao", "come", "otto", "acaca"]
-- 5
