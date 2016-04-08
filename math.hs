mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

mxxInt :: [Int] -> Int
mxxInt [] = error "empty list"
mxxInt [x] = x
mxxInt (x:xs) = max x (mxxInt xs)

-- Exercise 1.10
removeFst :: Int -> [Int] -> [Int]
removeFst x [] = []
removeFst x [y] | x == y = []
                | otherwise = [y]
removeFst x (y:ys) | y == x = ys
                   | otherwise = y : removeFst x ys

-- Example 1.11
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

srtInts' :: [Int] -> [Int] srtInts’ [] = []
srtInts' xs = let
                 m = mnmInt xs
              in m : (srtInts' (removeFst m xs))

-- Example 1.12
average :: [Int] -> Float
average [] = error "empty list"
avergae (xs) = fromIntegral (sum xs) / fromIntegral (length xs)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Exercise 1.13
count' :: (Eq a) => a -> [a] -> Int
count' x [] = 0
count' x (y:ys) | x == y = 1 + count' x ys
                | otherwise = count' x ys

-- Exercise 1.14
