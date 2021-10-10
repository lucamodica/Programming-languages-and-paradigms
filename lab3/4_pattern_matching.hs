-- ##Pattern matching##

-- es1
--[1, 2, 3] ++ []
--[1, 2] ++ [3]
--[1] ++ [2, 3]
--[] ++ [1, 2, 3]

-- es2
-- If ordinata func is written in this way:
--              ordinata xs
-- and not in the correct way:
--              ordinata (y : xs)
-- the result will be comparing by couple of
-- adjacent element, without checking relation
-- with any other numbers in the list.
-- For example, [2, 3, 1] == True where it would
-- be false.

-- es3
-- What happen if the following call of the func
-- stessaLunghezza:
--              stessaLunghezza [] [1..]
-- have to be computed?
-- Comparing the 2 different implementations of the
-- same above function:
-- 1.) In the first case, the one with the length
--     func is used, the computation will end up 
--     to fill all the memory trying to compute
--     the length of an infinite list (second
--     paramenter), where clearly the 2 lists
--     have different lengths.
-- 2.) In the second implementation instead the
--     empty list case is handled, so we can retrieve
--     the correct result.

-- es4
lProduct :: [Int] -> Int
lProduct []       = 1
lProduct (x : xs) = x * lProduct xs

-- es5
revert :: [Int] -> [Int]
revert []       = []
revert (x : xs) = revert xs ++ [x]

-- es6
jointSum :: [Int] -> [Int] -> [Int]
jointSum [] _              = []
jointSum _ []              = []
jointSum (x : xs) (y : ys) = [x + y] ++ jointSum xs ys


