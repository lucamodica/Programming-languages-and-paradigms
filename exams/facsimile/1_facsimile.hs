{-

Esercizio 1 (7 punti). Definire una funzione che, applicata a una lista xs, ritorna la sotto-lista
contenente tutti e soli gli elementi di xs in posizione pari, nello stesso ordine in cui compaiono in xs e
assumendo che il primo elemento della lista si trovi in posizione 0. È vietato fare uso di funzioni della
libreria standard ad eccezione di mod e quelle che hanno un nome simbolico, come +, ., ecc.

Esercizio 2 (7 punti). Ripetere l’esercizio precedente, questa volta senza fare uso esplicito della
ricorsione ma potendo usare tutte le funzioni definite nel modulo Prelude.

-}

-- es1
evenPosListRec :: [a] -> [a]
evenPosListRec (x : y : xs) = x : evenPosListRec xs
evenPosListRec xs = xs

-- es2
evenPosList :: [a] -> [a]
evenPosList = map snd . filter (even . fst) . zip [0 ..]
