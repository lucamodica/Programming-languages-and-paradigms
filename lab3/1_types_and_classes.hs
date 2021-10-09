-- ##Types and classes##

--es1
1 `div` 2 --well typed
1.5 `div` 2 --well typed
1.5 / 2 --well typed
(1 :: Int) / 2 --not well typed
(1 :: Float) / 2 --well typed
(2 :: Int) + (3 :: Integer) --not well typed
(2 :: Float) <= 3 --well typed
(2 :: Float) < (3 :: Int) --not well typed

--es2
fromInteger (1 :: Int) / 2