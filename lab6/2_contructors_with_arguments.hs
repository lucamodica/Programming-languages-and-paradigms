-- ##Constructors with arguments##

-- es1
data MaybeInt = Nothing | Just Int
    deriving (Eq, Show)

headd :: [Int] -> MaybeInt
head []      = Nothing
head (x : _) = Just x