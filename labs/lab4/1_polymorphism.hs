-- ##Polymorphism##

-- es1
fst (x, _)       = x -- fst :: (a, b) -> a
snd (_, y)       = y -- snd :: (a, b) -> b
const x _        = x -- const :: a -> b -> a
curry f x y      = f (x, y) -- curry :: ((a -> b) -> c) -> a -> b -> c
uncurry f (x, y) = f x y -- uncurry :: (a -> b -> c) -> (a -> b) -> c

-- es2
magia :: Int -> a
magia n = magia (n - 1)
-- This func can produce a result of any type, 
-- because it never ends.

-- es3
[a] -> Int -- list length
[a] -> Bool -- check if the list is empty
[a] -> a -- list head
[a] -> [a] -- list tail
[[a]] -> [a] -- list of lists
[a] -> Int -> a -- a list element in a specified index
Int -> [a] -> [a] -- a list of the first n elements of the parameter list
[a] -> [b] -> [(a, b)] -- a list of couple with element of the 2 lists in the same index
[(a, b)] -> ([a], [b]) -- a couple of the 2 lists formed with the list of couples

