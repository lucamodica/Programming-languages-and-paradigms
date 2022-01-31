{-

Esercizio 1 (7 punti). Dato il tipo algebrico
data Tree a = Empty | Node a [Tree a]
per rappresentare alberi n-ari, definire una funzione elements :: Tree a → [a] che calcola la
lista di tutti gli elementi contenuti nell’albero in un ordine a scelta. Usare la ricorsione solo laddove
necessario, sfruttando il più possibile le funzioni del modulo Prelude. Se opportuno, è ammessa la
definizione di funzioni ausiliarie.

Esercizio 2 (7 punti). In riferimento al tipo algebrico Tree dell’esercizio precedente, diciamo che
un albero è in forma normale se è Empty oppure se è costruito senza usare Empty. Definire una
funzione normalize :: Tree a → Tree a che trasforma un albero in forma normale, usando la
ricorsione solo laddove è necessario e sfruttando il più possibile le funzioni del modulo Prelude. Se
opportuno, è ammessa la definizione di funzioni ausiliarie.

-}

data Tree a = Empty | Node a [Tree a]
  deriving (Show, Eq)

-- es1
elements :: Tree a -> [a]
elements Empty = []
elements (Node x []) = [x]
elements (Node x (t : ts)) = x : concatMap elements ts

-- es2
normalize :: Tree a -> Tree a
normalize Empty = Empty
normalize (Node x ts) = Node x $ filter notEmpty (map normalize ts)
  where
    notEmpty Empty = False
    notEmpty _ = True
