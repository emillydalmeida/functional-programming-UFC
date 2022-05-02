final :: Int -> [a] -> [a]
final 0 [] = []
final a xs = drop (length xs - a) xs