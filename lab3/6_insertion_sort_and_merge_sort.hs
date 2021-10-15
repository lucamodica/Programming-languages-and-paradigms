-- ##Insertion sort and Merge sort##

-- es1
split :: [Int] -> ([Int], [Int])
split xs = (take n xs, drop n xs)
    where
        n = length `div` 2

-- es2
-- Without the following equation in the merge sort:
--              mergeSort [x] = [x]
-- the orderign algorithm cannot check lists of a single
-- elements, thus will ends an infinite recursion. 