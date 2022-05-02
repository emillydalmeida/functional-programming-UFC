calc :: Num a => a -> a -> a
calc x y = x * y
gangorra :: (Ord a, Num a, Num p) => a -> a -> a -> a -> p
gangorra a b c d
    | calc a b > calc c d = -1
    | calc a b < calc c d = 1
    | otherwise = 0 