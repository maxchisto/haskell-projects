skips :: [a] -> [[a]]
skips = \xs -> [skipN n (zip xs [1..]) | n <- [1..(length xs)]]

skipN :: Int -> [(a, Int)] -> [a]
skipN = (\n a-> [x | (x,i) <- a, (i `mod` n ==0) ])


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:[])
    | (x < y && z < y)  = y:[]
    | otherwise         = []
localMaxima (x:y:z:xs) 
    | (x < y && z < y)     = y:localMaxima(y:z:xs)
    | otherwise             = localMaxima(y:z:xs)