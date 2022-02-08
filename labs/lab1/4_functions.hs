-- ##functions##

--es1
dispari :: Int -> Bool
dispari x = mod x 2 /= 0

--es2
bisestile :: Int -> Bool
bisestile n = (mod n 4 == 0 && mod n 100 /= 0) || (mod n 400 == 0)

--es3
somma :: Int -> Int
somma n = div (n * (n + 1))  2

--es4
area :: Float -> Float
area a = 2 * pi * a

--es5
identity = not.not.dispari