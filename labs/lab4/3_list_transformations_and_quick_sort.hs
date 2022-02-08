-- ##List transformations and Quick Sort##

-- es1
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ x []       = x
foldl f x (y : ys) = foldl f (f x y) ys

-- es2
length . filter (>= 0) -- returns the number of positive numbers
foldr (&&) True . map (>= 0) -- check if all the numbers are positive
foldr (+) 0 . map (const 1) -- returns the size of the list
foldl (\xs x -> x : xs) [] -- basically reverse the list


-- es3
concat :: [[a]] -> [a]
concat = foldl (++) []

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (||) False . map p

all :: (a -> Bool) -> [a] -> Bool
all p = foldr (&&) True . map p


-- es4
massimo :: Ord a => [a] -> a
massimo (x : xs) = foldr max x xs

occorrenze :: Eq a => a -> [a] -> Int
occorrenze x = length . filter (== x)

membro :: Eq a => a -> [a] -> Bool
membro x = any (== x)