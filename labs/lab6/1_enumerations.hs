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


-- es3
-- Ordering is an enumeration that have 3 different values:
-- - LT (lower than)
-- - EQ (equal)
-- - GT (greater than).
-- Saying that the "compare" function, with 2 parameter values,
-- can return an Ordering value to indicates the comparison
-- result. We use a Ordering value where a comparison operation
-- may be expensive (for example between 2 lists).


-- es4
-- A case where the () type can be useful is in functions structured
-- such in this way: () -> T.
-- This is beacause "() -> T" will be a constant function, we already
-- know that it will be applied to a () type.
-- Functions such as "T -> ()" instead is useless beacause it can either
-- never ends or return a () type.





