-- ##Polymorph and recursive types##

-- es 1
data Maybee a = Nothingg | Justt a
  deriving (Eq, Show)

maybeLength :: Maybee a -> Int
maybeLength Nothingg = 0
maybeLength (Justt a) = 1

-- | >>>maybeLength (Nothingg)
maybeMap :: (a -> b) -> Maybee a -> Maybee b
maybeMap f Nothingg = Nothingg
maybeMap f (Justt a) = Justt (f a)

-- | >>>maybeMap (+1) (Justt 6)
maybeFilter :: (a -> Bool) -> Maybee a -> Maybee a
maybeFilter f (Justt a) | f a = Justt a
maybeFilter _ _ = Nothingg

-- | >>>maybeFilter (x > 0) (Justt 5)

-- es2
