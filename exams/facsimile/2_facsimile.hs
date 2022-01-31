{-

Esercizio 1 (7 punti). Definire nel modo più compatto possibile una funzione inversioni che calcola
il numero di inversioni di una lista, ovvero il numero di elementi immediatamente seguiti da un
elemento più grande. È vietato fare uso di funzioni della libreria standard ad eccezione di mod e
quelle che hanno un nome simbolico, come +, ., ecc. Fare in modo che inversioni abbia il tipo più
generale.

Esercizio 2 (7 punti). Ripetere l’esercizio precedente, questa volta senza fare uso esplicito della
ricorsione ma potendo usare tutte le funzioni definite nel modulo Prelude.

-}

-- es1
inversioniRec :: Ord a => [a] -> Int
inversioniRec [] = 0
inversioniRec [x] = 0
inversioniRec (x : y : xs)
  | x < y = 1 + inversioniRec (y : xs)
  | otherwise = inversioniRec (y : xs)

-- es2
inversioni :: Ord a => [a] -> Int
inversioni xs = length $ filter (uncurry (<=)) (zip xs (tail xs))
