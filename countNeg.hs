countNeg [] = 0
countNeg (x:xs)
    | x < 0 = 1 + countNeg xs
    | otherwise = countNeg xs