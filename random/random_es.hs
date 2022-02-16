-- useful data structures
data Tree a = Node a [Tree a]

data IntTree = Leaf Int | Nodee (Int, IntTree, IntTree)
  deriving (Show)

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

-- ## es14: given a list of strings xs computes the sum of the
-- lengths of the strings starting with the character 'A' ##
-- Recursive
totalLengthR :: [String] -> Int
totalLengthR [] = 0
totalLengthR (xs : xss)
  | startA xs = lengthh xs + totalLengthR xss
  | otherwise = totalLengthR xss
  where
    startA [] = False
    startA (x : xs)
      | x == 'A' = True
      | otherwise = False
    lengthh [] = 0
    lengthh (x : xs) = 1 + lengthh xs

-- Prelude
totalLengthC :: [String] -> Int
totalLengthC xs = sum $ map length $ filter ((== 'A') . head) xs

-- ## es15: given a list xs returns a new list obtained from xs
-- by removing the elements at odd positions ##
-- Recursive
filterOddR :: [a] -> [a]
filterOddR [] = []
filterOddR [x] = []
filterOddR (x : y : xs) = y : filterOddR xs

-- Prelude
filterOddC :: [a] -> [a]
filterOddC xs = map snd (filter (even . fst) (zip [1 ..] xs))

-- ## es16: implement tmap, a "tree version" of the map combinator.
-- More precisely, the function tmap should take a function f and a
-- tree t and should apply f to each value in t (use IntTree data) ##
tmap :: (Int -> Int) -> IntTree -> IntTree
tmap f (Leaf x) = Leaf (f x)
tmap f (Nodee (x, tx, ty)) = Nodee (f x, tmap f tx, tmap f ty)

-- ## es17: using tmap implement the function succTree taking a tree
-- t and computing a tree whose elements are the successors of the values
-- in t (use IntTree data) ##
succTree :: IntTree -> IntTree
succTree = tmap (+ 1)

-- ## es18: Write a function sumSucc taking a tree t and computing the
-- sum of the elements of succTree t (use IntTree data)
sumSucc :: IntTree -> Int
sumSucc = aux . succTree
  where
    aux (Leaf x) = x
    aux (Nodee (x, tx, ty)) = x + aux tx + aux ty

-- >>> succTree (Nodee (4, Leaf 7, Leaf 1))
-- Nodee (5,Leaf 8,Leaf 2)
