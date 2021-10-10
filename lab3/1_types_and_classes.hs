-- ##Types and classes##

-- es1
1 `div` 2 -- well typed
1.5 `div` 2 -- well typed
1.5 / 2 -- well typed
(1 :: Int) / 2 -- not well typed
(1 :: Float) / 2 -- well typed
(2 :: Int) + (3 :: Integer) -- not well typed
(2 :: Float) <= 3 -- well typed
(2 :: Float) < (3 :: Int) -- not well typed

-- es2
fromIntegral (1 :: Int) / 2
fromIntegral (2 :: Int) + (3 :: Integer)
round (2 :: Float) < (3 :: Int)

-- es3
(2 ^ 1024) :: Integer -- too big for an Integer type
(2 ^ 1024) :: Int -- too big for an Int type