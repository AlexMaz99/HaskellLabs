-- Funkcje wyższego rzędu: filter

import Data.Char

onlyEven [] = []
onlyEven (x:xs)
    | x `mod` 2 == 0 = x : onlyEven xs
    | otherwise      = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
    | x `mod` 2 /= 0 = x : onlyOdd xs
    | otherwise      = onlyOdd xs

onlyUpper [] = []
onlyUpper (x:xs)
    | x == toUpper x = x : onlyUpper xs
    | otherwise      = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] =[]
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

onlyEven' = filter (\x -> x `mod` 2 == 0)
onlyOdd' = filter (\x -> x `mod` 2 /= 0)
onlyUpper' = filter (\x -> x == toUpper x)

lengthEven = length (onlyEven [1..10^6])
lengthEven' = length . onlyEven $ [1..10^6]
lengthEven'' = length [x | x <- [1..10^6], even x]