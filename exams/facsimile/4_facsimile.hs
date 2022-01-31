{-

Esercizio 1 (7 punti). Definire una funzione che, applicata a una lista [a1, . . . , an] di numeri,
calcola sommatoria(2^i−1 · ai)
Per esempio, la funzione applicata alla lista [1, 0, 1] deve ritornare
2^0*1 + 2^1*0 + 2^2*1 = 5. È vietato fare uso di funzioni della libreria standard ad eccezione di
div, mod e di quelle che hanno un nome simbolico, come +, ., ecc.

Esercizio 2 (7 punti). Ripetere l’esercizio precedente, questa volta senza fare uso esplicito della
ricorsione ma potendo usare tutte le funzioni definite nel modulo Prelude.

-}

-- es1
sumExpRec :: [Int] -> Int
sumExpRec [] = 0
sumExpRec xs = aux 0 xs
  where
    aux i [] = 0
    aux i (x : xs) = (2 ^ i) * x + aux (i + 1) xs

-- es2
sumExp :: [Int] -> Int
sumExp xs = sum $ zipWith (*) (map (2 ^) [0 ..]) xs