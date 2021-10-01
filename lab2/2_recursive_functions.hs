-- ##recursive functions##

--es1
sumFirstN :: Int -> Int
sumFirstN 0 = 0
sumFirstN n = n + sumFirstN(n - 1)


--es2
pow2 :: Int -> Int
pow2 0 = 1
pow2 n = 2 * pow2(n - 1)


--es3
bits :: Int -> Int
bits n | n == 0 = 0
       | mod n 2 == 0 = bits(mod n 2)
       | otherwise = 1 + bits(mod n 2)

--es4
potenzaDi2 :: Int -> Bool
potenzaDi2 0 = False
potenzaDi2 1 = True
potenzaDi2 n = mod n 2 == 0 && potenzaDi2(div n 2)