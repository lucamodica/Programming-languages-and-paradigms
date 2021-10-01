-- ##anonymus function and section##

--es1
\x -> mod x 2 == 0 -- check even
\x -> mod x 2 /= 0 -- check odd


--es2
\x -> if x >= 0 then x else negate x


--es3
(< 10) -- Check if less than 10
(10 <) -- Check if 10 is less then a specific number
(`mod` 2) -- Compute the reminder of x/2
(1 /) -- Compute 1/x
(+ 1) . (* 2) -- Compute (x * 2) + 1
(* 2) . (+ 1) -- Compute (x + 1) * 2
(== 0) . (`mod` 2) -- Check if (x mod 2) == 0 (even number)
(/= 0) . (`mod` 2) -- Check if (x mod 2) != 0 (odd number)