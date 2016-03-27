mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

mxxInt :: [Int] -> Int
mxxInt [] = error "empty list"
mxxInt [x] = x
mxxInt (x:xs) = max x (mxxInt xs)

removeFst :: Int -> [Int] -> [Int]
removeFst x [] = []
removeFst x [y] | x == y = []
                | otherwise = [y]
removeFst x (y:ys) | y == x = ys
                   | otherwise = y : removeFst x ys

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs
