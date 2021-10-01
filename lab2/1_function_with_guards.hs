-- ##function with guards##

-- es1
assoluto :: Int -> Int
assoluto n | n >= 0 = n
           | n < 0 = negate n

nextAbs :: Int -> Int
--nextAbs n = if mod n 2 == 0 then n + 1 else assoluto n
nextAbs n | mod n 2 == 0 = n + 1
          | otherwise = assoluto n


--es2
bisestile :: Int -> Bool
bisestile n = (mod n 4 == 0 && mod n 100 /= 0) || (mod n 400 == 0)

giorni :: Int -> Int
giorni n | bisestile n = 366
         | otherwise = 365


--es3
assoluto2 :: Int -> Int
assoluto2 n | n >= 0 = n
            | otherwise = negate n

