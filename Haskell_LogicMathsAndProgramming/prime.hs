module GS

where

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n 

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k ^ 2 > n   = n
        | otherwise   = ldf (k+1) n

prime0 :: Integer -> Bool
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n

mnmInt :: [Int] -> Int
mnmInt []     = error "empty list"
mnmInt [x]    = x
mnmInt (x:xs) = min x (mnmInt xs)

min' :: Int -> Int -> Int
min' x y | x <= y    = x
         | otherwise = y

mxmInt :: [Int] -> Int
mxmInt []     = error "empty list"
mxmInt [x]    = x
mxmInt (x:xs) = max x (mxmInt xs) 

removeFst :: Int -> [Int] -> [Int]
removeFst _ [] = []
removeFst x (y:ys) | x == y    = ys
                   | otherwise = y : removeFst x ys

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

average :: [Int] -> Rational
average [] = error "empty list"
average xs = toRational (sum xs) / toRational (length xs)

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs