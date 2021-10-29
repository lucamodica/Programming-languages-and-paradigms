-- ##Enumerations##

-- es1
data PuntoCardinale = North | South | East | West
    deriving (Show, Eq)

left :: PuntoCardinale -> PuntoCardinale
left North = West
left West = South
left South = East
left East = North

back :: PuntoCardinale -> PuntoCardinale
back = left . left

right :: PuntoCardinale -> PuntoCardinale
right = back . left


-- es2
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Show, Eq, Ord, Enum)

tomorrow :: Day -> Day
tomorrow Mon = Tue
tomorrow Tue = Wed
tomorrow Wed = Thu
tomorrow Thu = Fri
tomorrow Fri = Sat
tomorrow Sat = Sun
tomorrow Sun = Mon

fra :: Int -> Day -> Day
fra 1 = tomorrow
fra n = tomorrow . fra (n - 1)

fraR :: Int -> Day -> Day
fraR n = foldr (.) id (replicate n tomorrow)




