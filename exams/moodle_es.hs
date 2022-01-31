-- es1
data Tree a = Empty | Node a [Tree a]
  deriving (Eq, Show)

normalize :: Tree a -> Tree a
normalize Empty = Empty
normalize (Node t ts) = Node t (filter nn (map normalize ts))
  where
    nn Empty = False
    nn _ = True

-- es2 (non-recursive)
evenPos :: [a] -> [a]
evenPos = map snd . filter (even . fst) . zip [0 ..]

-- es2 (recursive)
evenPosRec :: [a] -> [a]
evenPosRec [] = []
evenPosRec [x] = [x]
evenPosRec (x : y : xs) = x : evenPosRec xs

-- es3
numReverse :: [Int] -> Int
numReverse xs = length $ filter (uncurry (>)) $ zip [0 ..] $ tail xs

-- es4 (non-recursive)
lastEven :: Integral a => [a] -> Maybe a
lastEven [] = Nothing
lastEven xs = lastMaybe $ filter even xs
  where
    lastMaybe [] = Nothing
    lastMaybe xs = Just (last xs)

-- es5 (non-recursive)
allElemDiff :: Eq a => [a] -> [a] -> Bool
allElemDiff xs ys = any (`notElem` ys) xs

-- es5 (recursive)
allElemDiffRec :: Eq a => [a] -> [a] -> Bool
allElemDiffRec [] _ = False
allElemDiffRec (x : xs) ys = diffAll x ys || allElemDiffRec xs ys
  where
    diffAll _ [] = True
    diffAll x (y : ys) = (x /= y) && diffAll x ys

-- es6 (recursive)
