min :: Ord p => p -> p -> p
min x y
    | x < y = x
    | otherwise = y