-- ##Constructors with arguments##

-- es1
data MaybeInt = Nothingg | Justt Int
    deriving (Eq, Show)

headd :: [Int] -> MaybeInt
headd []      = Nothingg
headd (x : _) = Justt x


-- es2
data Numero = I Int | F Float
    deriving (Eq, Show)

somma :: Numero -> Numero -> Numero
somma (I x) (I y) = I (x + y)
somma (I x) (F y) = F (fromIntegral x + y)
somma (F x) (I y) = F (x + fromIntegral y)
somma (F x) (F y) = F (x + y)


-- es3
sommatoria :: [Numero] -> Numero
sommatoria = foldr somma (I 0)


-- es4
proprio :: [MaybeInt] -> [Int]
proprio []              = []
proprio (Nothingg : xs) = proprio xs
proprio (Justt x : xs)  = x : proprio xs